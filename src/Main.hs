module Main where

import Data.Array.Repa hiding ((++))
import Data.Functor
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