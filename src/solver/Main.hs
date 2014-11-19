module Main where

import Prelude as P
import Control.Monad.Random as Rand
import System.Environment (getArgs)

import Graphics.Solution
import Task
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
  task@(Task _ twrs _) <- loadTask input
  let solution = solve gen opts task
  saveResult output (filterTowers solution twrs)
  drawSolution task solution