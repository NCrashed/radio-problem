module Parsing where

import Data.Array.Repa (ix2)
import Language.Haskell.Interpreter

import Control.Monad
import System.IO

import Task

fromInterpreter :: FilePath -> String -> Interpreter a -> IO a
fromInterpreter file moduleName intf = do
  res <- runInterpreter intf'
  case res of
    Left err -> fail $ show err
    Right val -> return val
  where
    intf' = do
      loadModules [file]
      setImports ["Prelude", moduleName] 
      intf
      
loadFitnessFunc :: FilePath -> IO FitnessFunc
loadFitnessFunc file = fromInterpreter file "Fitness" $ interpret "fitness" (as :: FitnessFunc)
  
-- | Loading Task from file and script file
loadTask :: FilePath -> FilePath -> IO Task
loadTask inputScript fitnessScript = do
  fitnessFunc <- loadFitnessFunc fitnessScript
  task <- fromInterpreter inputScript "Input" $ do
    radius <- interpret "radius" (as :: Int)
    (w, h) <- interpret "fieldSize" (as :: (Int, Int))
    towers <- interpret "towers" (as :: [(Int, Int)])
    return $ Task (ix2 w h) towers radius
  return $ task fitnessFunc
  
loadEvolOptions :: FilePath -> IO EvolOptions
loadEvolOptions path = fromInterpreter path "Options" $ do
  mutChance <- interpret "mutationChance" (as :: Float)
  checkf "Mutation chance" mutChance
  elite <- interpret "elitePart" (as :: Float)
  checkf "Elite part" elite
  maxGen <- interpret "maxGeneration" (as :: Int)
  checki "Maximum number of generations" maxGen
  populationCount <- interpret "populationCount" (as :: Int)
  checki "Count of populations" populationCount
  individsCount <- interpret "individsCount" (as :: Int)
  checki "Count of individs in single population" individsCount
  return $ EvolOptions mutChance elite maxGen populationCount individsCount
  where
    checkf str v = when (v < 0 || v > 1.0) $ fail $ "Failed to load inputs, " ++ str 
      ++ " must be in range of [0.0, 1.0]!"
    checki str v = when (v <= 0) $ fail $ "Failt to load inputs, " ++ str
      ++ " must be positive!"

-- | Saving answer to file
saveResult :: FilePath -> [Tower] -> IO ()
saveResult path ts = withFile path WriteMode $ \h -> mapM_ (\(x,y) -> hPutStrLn h $ show x ++ " " ++ show y) ts 
