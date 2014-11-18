module Main where

import Prelude as P
import Data.Functor
import Control.Monad.Random as Rand
import System.Environment (getArgs)

import Parsing
import Genetic
           
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
  gen <- newStdGen
  saveResult output =<< solve gen opts <$> loadTask input