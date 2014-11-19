module Main where

import Options.Applicative

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
  <*> option auto (
       long "output"
    <> short 'o'
    <> metavar "FILENAME"
    <> help "where to save generated data set")
  
generator :: Options -> IO ()
generator = undefined

main :: IO ()
main = execParser opts >>= generator
  where
    opts = info (helper <*> options) (
         fullDesc
      <> progDesc "Generates data set with COUNT towers and field of WIDTH x HEIGHT size, stores output to FILENAME file location"
      <> header "radio-problem-generator - generator of data sets for 'radio-problem'" )