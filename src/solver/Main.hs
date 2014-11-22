module Main where

import Prelude as P
import Control.Monad.Random as Rand
import System.Environment (getArgs)
import Data.Functor
import Data.IORef
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Solution
import Graphics.Plot

import Args
import Task
import Parsing
import Genetic

import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Control.Arrow (first)


main :: IO ()
main = do
  args <- parseOptions 
  
  opts <- loadEvolOptions (evolOptionsFileName args)
  gen <- newStdGen
  task@(Task _ twrs _ _) <- loadTask (inputFileName args) (fitnessFuncFileName args)
  
  chan <- newTChanIO
  asolution <- async $ solve chan gen opts task
  dataRef <- newIORef []
  finalSolutionRef <- newIORef Nothing
  
  animateIO mode white $ const $ do
      mfinsol <- readIORef finalSolutionRef
      case mfinsol of
        Just solution -> do
          samples <- readIORef dataRef
          return $ solutionPicture task solution (fitnessPlot samples)
        Nothing -> do
          msolution <- poll asolution     
          case msolution of
            Nothing -> do
              mv <- atomically $ tryReadTChan chan
              case mv of
                Nothing -> return ()
                Just v -> modifyIORef dataRef (++[v])
              samples <- readIORef dataRef
              return $ fitnessPlot samples
            Just esol -> case esol of
              Left e -> fail $ show e
              Right solution -> do
                saveResult (outputFileName args) (filterTowers solution twrs)
                writeIORef finalSolutionRef (Just solution)
                samples <- readIORef dataRef
                return $ solutionPicture task solution (fitnessPlot samples)
      where mode = InWindow "Radio-problem solver" (1280, 1024) (10, 10)
            fitnessPlot ds =  translate (-300) (-300) $ scale 600 600 $ plot "generation" "fitness" $ first fromIntegral <$> ds