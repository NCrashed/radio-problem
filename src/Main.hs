module Main where

import Prelude as P
import Data.Array.Repa hiding ((++))
import Data.Functor
import Data.List
import qualified Data.Vector as Vec 
import Control.Monad.Random
import Control.Monad
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import System.Environment (getArgs)
import System.IO

-- | Field is two dimensional array of unboxed booleans
type Field = Array U DIM2 Bool 

-- | Tower is a point in Field
type Tower = (Int, Int)
-- | Radius of network generated with single tower
type Radius = Int

-- | Input data
data Task = Task Field [Tower] Radius

-- | Options of evolution process
data EvolOptions = EvolOptions {
    mutationChance :: Float
  , crossoverChance :: Float
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
      let emptyField = computeUnboxedS $ fromFunction (ix2 n m) (const False)
      return $ Task emptyField towers radius
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
      cros <- mfloat "Crossingover chance"
      spaces
      elite <- mfloat "Elite part"
      spaces
      maxgen <- int
      spaces
      popc <- int
      spaces
      indc <- int
      return $ EvolOptions mut cros elite maxgen popc indc
    floating = float $ makeTokenParser haskellDef 
    mfloat msg = do
        f <- floating
        when (not $ f >= 0 && f <= 1.0) $ fail $ msg ++ " must be in range [0.0 .. 1.0]"
        return (fromRational $ toRational f)
          
-- | Saving answer to file
saveResult :: FilePath -> [Tower] -> IO ()
saveResult path ts = withFile path WriteMode $ \h -> mapM_ (\(x,y) -> hPutStrLn h $ show x ++ " " ++ show y) ts 

-- | Solving the problem using genetic algorithm
solve :: EvolOptions -> Task -> [Tower]
solve opts (Task field towers radius) = undefined

type Chromosome = Vec.Vector Bool
type Population = [Chromosome]

-- | Creating chromosome with random values, n is a length of chromosome
initChromosome :: Int -> Rand StdGen Chromosome
initChromosome n = Vec.replicateM n randBool
  where randBool :: Rand StdGen Bool
        randBool = liftRand $ random 

-- | Creating population with m chromosomes with length n
initPopulation :: Int -> Int -> Rand StdGen Population
initPopulation m n = sequence $ replicate m $ initChromosome n

filterTowers :: Chromosome -> [Tower] -> [Tower]
filterTowers chr = P.map snd . filter ((== True) . fst) . zip (Vec.toList chr)

toFloat :: Int -> Float
toFloat = fromIntegral . toInteger

-- | Calculates percentage of network coverage by solution in chromosome
calcCoverage :: Chromosome -> Field -> Radius -> [Tower] -> Float
calcCoverage chr field radius = coverage . foldl' placeTower field . filterTowers chr
  where 
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
fitness :: Chromosome -> Field -> Radius -> [Tower] -> Float
fitness chr field radius twrs = (coverage + minimal) / 2.0
  where coverage = calcCoverage chr field radius twrs
        minimal = toFloat (length $ filterTowers chr twrs) / toFloat (length twrs)

-- | Caclulates next generation of population
nextPopulation :: EvolOptions -> Population -> Rand StdGen Population
nextPopulation opts pop = undefined
        
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
  saveResult output =<< solve opts <$> loadTask input