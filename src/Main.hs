module Main where

import Prelude as P
import Data.Array.Repa hiding ((++))
import Data.Functor
import Data.List
import qualified Data.Vector as Vec 
import Control.Monad.Random
import Text.Parsec
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
    
-- | Saving answer to file
saveResult :: FilePath -> [Tower] -> IO ()
saveResult path ts = withFile path WriteMode $ \h -> mapM_ (\(x,y) -> hPutStrLn h $ show x ++ " " ++ show y) ts 

-- | Solving the problem using genetic algorithm
solve :: Task -> [Tower]
solve (Task field towers radius) = undefined

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
        
-- | Reading input and output filenames from program arguments
parseArgs :: IO (String, String)
parseArgs = do
  ls <- getArgs
  if length ls < 2 
    then fail "Expecing two arguments: names of input file and output file"
    else return (head ls, ls !! 1)
    
main :: IO ()
main = do
  (input, output) <- parseArgs 
  saveResult output =<< solve <$> loadTask input