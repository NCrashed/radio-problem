module Main where

import Options.Applicative
import Control.Monad.Random
import Control.Monad
import System.IO

import Data.Encoding
import Data.Encoding.CP1251
import Data.Encoding.UTF8

e :: String -> String
e = encodeString CP1251 . decodeString UTF8

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
    <> help (e "минимальный радиус башни")
    <> value 3)
  <*> option auto (
       long "radiusMax"
    <> metavar "value"
    <> help (e "максимальный радиус башни")
    <> value 7)
  <*> option auto (
       long "fieldWidth"
    <> short 'w'
    <> metavar "WIDTH"
    <> help (e "ширина поля сети"))
  <*> option auto (
       long "fieldHeight"
    <> short 'h'
    <> metavar "HEIGHT"
    <> help (e "высота поля сети"))
  <*> option auto (
       long "towersCount"
    <> short 'n'
    <> metavar "COUNT"
    <> help (e "сколько генерировать возможных точек установки радио-вышек"))
  <*> strOption (
       long "output"
    <> short 'o'
    <> metavar "FILENAME"
    <> help (e "в какой файл сохранить результат генерации условия"))
    
main :: IO ()
main = execParser opts >>= generator
  where
    opts = info (helper <*> options) (
         fullDesc
      <> progDesc (e "Создает входные данные для программы 'radio-problem' с количеством COUNT возможных точек установки радио-вышек и полем WIDTH на HEIGHT клеток, сохраняет вывод в файл с именем FILENAME")
      <> header (e "radio-problem-generator - генератор входных данных для 'radio-problem'") )

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