module Main where

import Options.Applicative
import Control.Monad.Random
import Control.Monad
import System.IO

data Options = Options {
    radiusMin :: Int
  , radiusMax :: Int
  , fieldWidth :: Int
  , fieldHeight :: Int
  , towersCount :: Int
  , outputFile :: String
}

options :: Parser Options
options = Options 
  <$> option auto (
       long "radiusMin"
    <> metavar "value"
    <> help "minimum radius of tower" 
    <> value 3)
  <*> option auto (
       long "radiusMax"
    <> metavar "value"
    <> help "maximum radius of tower"
    <> value 7)
  <*> option auto (
       long "fieldWidth"
    <> short 'w'
    <> metavar "WIDTH"
    <> help "width of network field")
  <*> option auto (
       long "fieldHeight"
    <> short 'h'
    <> metavar "HEIGHT"
    <> help "height of network field")
  <*> option auto (
       long "towersCount"
    <> short 'n'
    <> metavar "COUNT"
    <> help "how many towers positions to generate")
  <*> strOption (
       long "output"
    <> short 'o'
    <> metavar "FILENAME"
    <> help "where to save generated data set")
    
main :: IO ()
main = execParser opts >>= generator
  where
    opts = info (helper <*> options) (
         fullDesc
      <> progDesc "Generates data set with COUNT towers and field of WIDTH x HEIGHT size, stores output to FILENAME file location"
      <> header "radio-problem-generator - generator of data sets for 'radio-problem'" )

generator :: Options -> IO ()
generator o = saveTask (outputFile o) =<< evalRandIO (generate o)

type FieldSize = (Int, Int)
type Tower = (Int, Int)
type Radius = Int
data Task = Task FieldSize [Tower] Radius

generate :: Options -> Rand StdGen Task
generate o = do 
  radius <- getRandomR (radiusMin o, radiusMax o)
  towers <- replicateM (towersCount o) $ do
    x <- getRandomR (0, fieldWidth o - 1)
    y <- getRandomR (0, fieldHeight o - 1)
    return (x, y)
  return $ Task (fieldWidth o, fieldHeight o) towers radius

saveTask :: FilePath -> Task -> IO ()
saveTask path (Task fieldSize twrs radius) = withFile path WriteMode $ \h -> do
  hPutStrLn h "module Input where\n"
  hPutStrLn h "radius :: Int"
  hPutStrLn h $ "radius = " ++ show radius ++ "\n"
  hPutStrLn h "fieldSize :: (Int, Int)"
  hPutStrLn h $ "fieldSize = " ++ show fieldSize ++ "\n"
  hPutStrLn h "towers :: [(Int, Int)]"
  hPutStrLn h "towers = ["
  printTowers h twrs 
  hPutStrLn h "  ]"
  where
    printTowers _ [] = return ()
    printTowers h [x] = hPutStrLn h $ "  " ++ show x
    printTowers h (x:ls) = hPutStrLn h ("  " ++ show x ++ ",") >> printTowers h ls  