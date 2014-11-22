module Parsing where

import Data.Array.Repa hiding ((++))
import Data.Functor

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Control.Monad
import System.IO

import Task
import Plugin

-- | Loading Task from file and script file
loadTask :: FilePath -> FilePath -> IO Task
loadTask path script = do
  str <- readFile path
  fitnessFunc <- loadFitnessFunc script
  case parse parser "" str of
    Left err -> fail $ show err
    Right task -> return $ task fitnessFunc
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
