module Genetic where

import Prelude as P
import qualified Data.Vector as Vec
import Data.Array.Repa hiding ((++))
import Data.Function
import Data.Functor
import Data.List as List
import Control.Arrow 
import Control.Monad.Random as Rand
import Control.Monad as Monad

import Task

newtype Chromosome = Chromosome (Vec.Vector Bool)
type Population = [Chromosome]

instance Show Chromosome where
  show (Chromosome chr) = concat . Vec.toList $ (\b -> if b then "1" else "0") <$> chr
  
-- | Solving the problem using genetic algorithm
solve :: StdGen -> EvolOptions -> Task -> Chromosome
solve gen opts task@(Task _ twrs _) = evalRand solve' gen
  where 
    solve' = do
      pops <- Monad.replicateM (popCount opts) $ 
        initPopulation (indCount opts) (length twrs)
      lastPop <- foldM nextGen pops $ reverse [1 .. maxGeneration opts]
      return (snd $ findBest task lastPop)
    
    nextGen :: [Population] -> Int -> Rand StdGen [Population]
    nextGen pops _ = mapM (nextPopulation opts task) pops 
      -- TODO: Put this in writer monad
      -- trace ("Generation " ++ show i)
      -- trace ("Best fitness: " ++ show (fst $ findBest task pops'))
      
-- | Fetching best solution from populations      
findBest :: Task -> [Population] -> (Float, Chromosome)
findBest task pops = maximumBy (compare `on` fst) 
  $ findPopBest task <$> pops

-- | Fetching best solution from population
findPopBest :: Task -> Population -> (Float, Chromosome)
findPopBest task pop = maximumBy (compare `on` fst) $ first (fitness task) <$> zip pop pop
    
-- | Creating chromosome with random values, n is a length of chromosome
initChromosome :: Int -> Rand StdGen Chromosome
initChromosome n = Chromosome <$> Vec.replicateM n randBool
  where randBool :: Rand StdGen Bool
        randBool = liftRand random 

-- | Creating population with m chromosomes with length n
initPopulation :: Int -> Int -> Rand StdGen Population
initPopulation m n = replicateM m $ initChromosome n

-- | Returns only placed towers by solution in chromosome
filterTowers :: Chromosome -> [Tower] -> [Tower]
filterTowers (Chromosome chr) = P.map snd . filter ((== True) . fst) . zip (Vec.toList chr)

-- | Conversion helper
toFloat :: Int -> Float
toFloat = fromIntegral . toInteger

-- | Constructs network field from solution in chromosome
solutionField :: Task -> Chromosome -> Field
solutionField (Task fsize twrs radius) chr = foldl' placeTower field $ filterTowers chr twrs
  where 
    field = cleanField fsize
    placeTower :: Field -> Tower -> Field 
    placeTower (Field f) (tx, ty) = Field $ computeUnboxedS $ traverse f id $ 
      \getter sh -> getter sh + if inRadius sh then 1 else 0 
      where
        inRadius :: DIM2 -> Bool
        inRadius (Z :. y :. x) = (tx-x)*(tx-x) + (ty-y)*(ty-y) <= radius*radius

-- | Calculates percentage of network coverage by solution in chromosome
calcCoverage :: Task -> Chromosome -> Float
calcCoverage task = coverage . solutionField task
  where 
    coverage :: Field -> Float
    coverage (Field f) = toFloat covered / toFloat area
      where
        covered = foldl' (\i b -> if b>0 then i+1 else i) 0 (toList f)
        area = size $ extent f

-- | Calculates fitness for solution stored in chromosome
fitness :: Task -> Chromosome -> Float
fitness task@(Task _ twrs _) chr = coverage * minimal
  where coverage = calcCoverage task chr
        towersUsed = length $ filterTowers chr twrs
        towersCount = length twrs
        minimal = 1 - (toFloat towersUsed / toFloat towersCount)

-- | Helper to choose between two elements with provided chance of the first one
randChoice :: Rational -> Rand StdGen a -> Rand StdGen a -> Rand StdGen a
randChoice chance th els = join (Rand.fromList [(th, chance), (els, 1 - chance)])

-- | Caclulates next generation of population
nextPopulation :: EvolOptions -> Task -> Population -> Rand StdGen Population
nextPopulation opts task pop = do 
  newPop <- liftM concat $ Monad.replicateM (length pop `div` 2) $ do
    a1 <- takeChr
    b1 <- takeChr
    (a2, b2) <- crossover a1 b1
    a3 <- applyMutation a2
    b3 <- applyMutation b2
    return [a3, b3]
  return $ if length newPop <= length pop then newPop else tail newPop
  where fits = toRational <$> fitness task <$> pop
        maxfit = maximum fits
        chances = zip pop ((/maxfit) <$> fits)
        takeChr = Rand.fromList chances
        mutChance = toRational $ mutationChance opts
        applyMutation c = randChoice mutChance (mutation c) (return c)

-- | Crossover operator, fallbacks to trival cases if length isn't enough for
-- thre pointed crossover
crossover :: Chromosome -> Chromosome -> Rand StdGen (Chromosome, Chromosome)
crossover ca@(Chromosome a) cb@(Chromosome b) 
  | Vec.length a <= 1 = return (ca, cb)
  | Vec.length a == 2 = return (Chromosome $ a Vec.// [(1, b Vec.! 1)], Chromosome $ b Vec.// [(0, a Vec.! 0)])
  | otherwise         = crossover3 ca cb
  
-- | Implements three point crossover. Length of chromosome must be > 3
crossover3 :: Chromosome -> Chromosome -> Rand StdGen (Chromosome, Chromosome)
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
mutation :: Chromosome -> Rand StdGen Chromosome
mutation (Chromosome a) = do
  i <- getRandomR (0, Vec.length a - 1)
  return $ Chromosome $ a Vec.// [(i, not $ a Vec.! i)]