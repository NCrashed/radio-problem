module Genetic where

import Prelude as P
import qualified Data.Vector as Vec
import Data.Function
import Data.Functor
import Data.List as List
import Control.Arrow 
import Control.Monad.Random as Rand
import Control.Monad as Monad
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)

import Task

type GenRand = RandT StdGen IO 
  
-- | Solving the problem using genetic algorithm
solve :: StatChannel -> StdGen -> EvolOptions -> Task -> IO Chromosome
solve ch gen opts task@(Task _ twrs _ _) = evalRandT solve' gen
  where 
    solve' = do
      pops <- Monad.replicateM (popCount opts) $ do
        liftIO yield
        initPopulation (indCount opts) (length twrs)
      lastPop <- foldM nextGen pops [1 .. maxGeneration opts]
      return (snd $ findBest task lastPop)
    
    nextGen :: [Population] -> Int -> GenRand [Population]
    nextGen pops i = do
      pops' <- mapM (nextPopulation opts task) pops 
      liftIO $ atomically $ writeTChan ch (i, fst $ findBest task pops')
      return pops'
      
-- | Fetching best solution from populations      
findBest :: Task -> [Population] -> (Float, Chromosome)
findBest task pops = maximumBy (compare `on` fst) 
  $ findPopBest task <$> pops

-- | Fetching best solution from population
findPopBest :: Task -> Population -> (Float, Chromosome)
findPopBest task pop = maximumBy (compare `on` fst) $ first (fitness task) <$> zip pop pop
    
-- | Creating chromosome with random values, n is a length of chromosome
initChromosome :: Int -> GenRand Chromosome
initChromosome n = Chromosome <$> Vec.replicateM n randBool
  where randBool :: GenRand Bool
        randBool = liftRandT (return <$> random) 

-- | Creating population with m chromosomes with length n
initPopulation :: Int -> Int -> GenRand Population
initPopulation m n = replicateM m $ initChromosome n

-- | Helper to choose between two elements with provided chance of the first one
randChoice :: Rational -> GenRand a -> GenRand a -> GenRand a
randChoice chance th els = join (Rand.fromList [(th, chance), (els, 1 - chance)])

-- | Caclulates next generation of population
nextPopulation :: EvolOptions -> Task -> Population -> GenRand Population
nextPopulation opts task pop = do 
  newPop' <- liftM concat $ Monad.replicateM (nonEliteCount `div` 2) $ do
    liftIO yield
    a1 <- takeChr
    b1 <- takeChr
    (a2, b2) <- crossover a1 b1
    a3 <- applyMutation a2
    b3 <- applyMutation b2
    return [a3, b3]
  let newPop = elite ++ newPop'
  return $ if length newPop <= length pop then newPop else tail newPop
  where fits = toRational <$> fitness task <$> pop
        maxfit = maximum fits
        chances = zip pop ((/maxfit) <$> fits)
        takeChr = Rand.fromList chances
        mutChance = toRational $ mutationChance opts
        applyMutation c = randChoice mutChance (mutation c) (return c)
        bests = snd <$> sortBy (flip compare `on` fst) (first (fitness task) <$> zip pop pop)
        elite = take (ceiling $ fromIntegral (length bests) * elitePart opts) bests
        nonEliteCount = length pop - length elite
        
-- | Crossover operator, fallbacks to trival cases if length isn't enough for
-- thre pointed crossover
crossover :: Chromosome -> Chromosome -> GenRand (Chromosome, Chromosome)
crossover ca@(Chromosome a) cb@(Chromosome b) 
  | Vec.length a <= 1 = return (ca, cb)
  | Vec.length a == 2 = return (Chromosome $ a Vec.// [(1, b Vec.! 1)], Chromosome $ b Vec.// [(0, a Vec.! 0)])
  | otherwise         = crossover3 ca cb
  
-- | Implements three point crossover. Length of chromosome must be > 3
crossover3 :: Chromosome -> Chromosome -> GenRand (Chromosome, Chromosome)
crossover3 (Chromosome a) (Chromosome b)  = do
  [p1, p2, p3] <- sort <$> Monad.replicateM 3 (getRandomR (1, n - 2))
  let a' = Vec.concat [firsts p1 a, middles p1 p2 b, middles p2 p3 a, lasts p3 b]
  let b' = Vec.concat [firsts p1 b, middles p1 p2 a, middles p2 p3 b, lasts p3 a]
  return (Chromosome a', Chromosome b')
  where
    n = Vec.length a
    firsts = Vec.slice 0
    lasts p = Vec.slice p (n - p)
    middles p1 p2 = Vec.slice p1 (p2 - p1)
    
-- | Implements mutation of one bit
mutation :: Chromosome -> GenRand Chromosome
mutation (Chromosome a) = do
  i <- getRandomR (0, Vec.length a - 1)
  return $ Chromosome $ a Vec.// [(i, not $ a Vec.! i)]