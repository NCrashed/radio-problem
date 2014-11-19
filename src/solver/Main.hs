module Main where

import Prelude as P
import Control.Monad.Random as Rand
import System.Environment (getArgs)
import Data.Functor
import Data.IORef
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Solution
import Graphics.Plot
import Task
import Parsing
import Genetic

import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Control.Arrow (first)

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
  
  chan <- newTChanIO
  asolution <- async $ solve chan gen opts task
  dataRef <- newIORef []
  
  animateIO mode white $ const $ do
      msolution <- poll asolution
      case msolution of
        Nothing -> do
          mv <- atomically $ tryReadTChan chan
          case mv of
            Nothing -> return ()
            Just v -> modifyIORef dataRef (++[v])
          samples <- readIORef dataRef
          return $ translate (-400) (-400) $ scale 800 800 $ plot "generation" "fitness" $ first fromIntegral <$> samples
        Just esol -> case esol of
          Left e -> fail $ show e
          Right solution -> do
            saveResult output (filterTowers solution twrs)
            return $ solutionPicture task solution
  where mode = InWindow "Radio-problem solver" (1280, 1024) (10, 10)