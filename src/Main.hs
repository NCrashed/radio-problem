module Main where

import Prelude as P
import Data.Array.Repa hiding ((++))
import Data.Functor
import Data.List as List
import qualified Data.Vector as Vec 
import Control.Monad.Random as Rand
import Control.Monad as Monad
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import System.Environment (getArgs)
import System.IO

-- | Field is two dimensional array of unboxed booleans
type Field = Array U DIM2 Bool
-- | It's not necessary to carry whole field around
-- we can just pass field size to create a new field
-- for needed calculations 
type FieldSize = DIM2

-- | Creating of empty field
cleanField :: FieldSize -> Field
cleanField s = computeUnboxedS $ fromFunction s (const False)

-- | Tower is a point in Field
type Tower = (Int, Int)
-- | Radius of network generated with single tower
type Radius = Int

-- | Input data
data Task = Task FieldSize [Tower] Radius

-- | Options of evolution process
data EvolOptions = EvolOptions {
    mutationChance :: Float
  , elitePart :: Float
  , maxGeneration :: Int
  , popCount :: Int
  , indCount :: Int
  }

-- | Loading Task from file
loadTask :: FilePath -> IO Task
loadTask path = do
  str <- readFile path
  case parse parser "" str of
    Left err -> fail $ show err
    Right task -> return task
  where
    parser = do
      spaces
      radius <- int
      spaces
      (n,m) <- int2
      spaces
      towers <- many $ do
        spaces
        coords <- int2
        spaces
        return coords
      return $ Task (ix2 n m) towers radius
    int2 = do
      a <- int
      spaces
      b <- int
      return (a,b)
      
int :: Parsec String () Int
int = read <$> many1 digit
  
loadEvolOptions :: FilePath -> IO EvolOptions
loadEvolOptions path = do
  str <- readFile path
  case parse parser "" str of
    Left err -> fail $ show err
    Right task -> return task
  where
    parser = do
      spaces
      mut <- mfloat "Mutation chance"
      spaces
      elite <- mfloat "Elite part"
      spaces
      maxgen <- mint "Max generation"
      spaces
      popc <- mint "Populations count"
      spaces
      indc <- mint "Individs count"
      return $ EvolOptions mut elite maxgen popc indc
    floating = float $ makeTokenParser haskellDef 
    mfloat msg = do
      f <- floating
      unless (f >= 0 && f <= 1.0) $ fail $ msg ++ " must be in range [0.0 .. 1.0]"
      return (fromRational $ toRational f)
    mint msg = do
      i <- int
      unless (i > 0) $ fail $ msg ++ " must be positive!" 
      return i
        
-- | Saving answer to file
saveResult :: FilePath -> [Tower] -> IO ()
saveResult path ts = withFile path WriteMode $ \h -> mapM_ (\(x,y) -> hPutStrLn h $ show x ++ " " ++ show y) ts 

-- | Solving the problem using genetic algorithm
solve :: StdGen -> EvolOptions -> Task -> [Tower]
solve gen opts (Task field towers radius) = undefined

type Chromosome = Vec.Vector Bool
type Population = [Chromosome]

-- | Creating chromosome with random values, n is a length of chromosome
initChromosome :: Int -> Rand StdGen Chromosome
initChromosome n = Vec.replicateM n randBool
  where randBool :: Rand StdGen Bool
        randBool = liftRand random 

-- | Creating population with m chromosomes with length n
initPopulation :: Int -> Int -> Rand StdGen Population
initPopulation m n = replicateM m $ initChromosome n

-- | Returns only placed towers by solution in chromosome
filterTowers :: Chromosome -> [Tower] -> [Tower]
filterTowers chr = P.map snd . filter ((== True) . fst) . zip (Vec.toList chr)

-- | Conversion helper
toFloat :: Int -> Float
toFloat = fromIntegral . toInteger

-- | Calculates percentage of network coverage by solution in chromosome
calcCoverage :: Task -> Chromosome -> Float
calcCoverage (Task fsize twrs radius) chr = coverage $ foldl' placeTower field $ filterTowers chr twrs
  where 
    field = cleanField fsize
    placeTower :: Field -> Tower -> Field 
    placeTower f (tx, ty) = computeUnboxedS $ traverse f id $ \_ sh -> inRadius sh 
      where
        inRadius :: DIM2 -> Bool
        inRadius (Z :. y :. x) = (tx-x)*(tx-x) + (ty-y)*(ty-y) <= radius*radius
    coverage :: Field -> Float
    coverage f = toFloat covered / toFloat area
      where
        covered = foldl' (\i b -> if b then i+1 else i) 0 (toList f)
        area = size $ extent f

-- | Calculates fitness for solution stored in chromosome
fitness :: Task -> Chromosome -> Float
fitness task@(Task _ twrs _) chr = (coverage + minimal) / 2.0
  where coverage = calcCoverage task chr
        minimal = toFloat (length $ filterTowers chr twrs) / toFloat (length twrs)

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
  return $ if length newPop == length pop then newPop else tail newPop
  where fits = toRational <$> fitness task <$> pop
        maxfit = maximum fits
        chances = zip pop ((/maxfit) <$> fits)
        takeChr = Rand.fromList chances
        mutChance = toRational $ mutationChance opts
        applyMutation c = randChoice mutChance (mutation c) (return c)

-- | Crossover operator, fallbacks to trival cases if length isn't enough for
-- thre pointed crossover
crossover :: Chromosome -> Chromosome -> Rand StdGen (Chromosome, Chromosome)
crossover a b 
  | Vec.length a <= 1 = return (a, b)
  | Vec.length a == 2 = return (a Vec.// [(1, b Vec.! 1)], b Vec.// [(0, a Vec.! 0)])
  | otherwise         = crossover3 a b
  
-- | Implements three point crossover. Length of chromosome must be > 3
crossover3 :: Chromosome -> Chromosome -> Rand StdGen (Chromosome, Chromosome)
crossover3 a b = do
  [p1, p2, p3] <- sort <$> Monad.replicateM 3 (getRandomR (1, n - 2))
  let a' = Vec.concat [firsts p1 a, middles p1 p2 b, middles p2 p3 a, lasts p3 b]
  let b' = Vec.concat [firsts p1 b, middles p1 p2 a, middles p2 p3 b, lasts p3 a]
  return (a', b')
  where
    n = Vec.length a
    firsts = Vec.slice 0
    lasts p = Vec.slice p (n - p)
    middles p1 p2 = Vec.slice p1 (p2 - p1)
    
-- | Implements mutation of one bit
mutation :: Chromosome -> Rand StdGen Chromosome
mutation a = do
  i <- getRandomR (0, Vec.length a - 1)
  return $ a Vec.// [(i, not $ a Vec.! i)]
           
-- | Reading input and output filenames from program arguments
parseArgs :: IO (String, String, String)
parseArgs = do
  ls <- getArgs
  if length ls < 3 
    then fail "Expecing three arguments: name of input file, name of output file and name of file with evolution options"
    else return (head ls, ls !! 1, ls !! 2)
    
main :: IO ()
main = do
  (input, output, evolopts) <- parseArgs 
  opts <- loadEvolOptions evolopts
  gen <- getStdGen
  saveResult output =<< solve gen opts <$> loadTask input