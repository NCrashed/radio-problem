module Plugin where

import Language.Haskell.Interpreter
import Task

loadFitnessFunc :: FilePath -> IO FitnessFunc
loadFitnessFunc file = do
  res <- runInterpreter $ interpretFitnessFunc file
  case res of
    Left err -> fail $ show err
    Right val -> return val

interpretFitnessFunc :: FilePath -> Interpreter FitnessFunc
interpretFitnessFunc file = do
  loadModules [file]
  setImports ["Prelude", "Fitness"]
  interpret "fitness" (as :: FitnessFunc)