module Args where

import Options.Applicative

data Options = Options {
    inputFileName :: String
  , outputFileName :: String
  , evolOptionsFileName :: String
  , fitnessFuncFileName :: String
}

options :: Parser Options
options = Options 
  <$> strOption (
       long "input"
    <> short 'i'
    <> metavar "INPUT_FILE_NAME"
    <> help "file with input data for model" 
    <> value "input.txt")
  <*> strOption (
       long "output"
    <> short 'o'
    <> metavar "OUTPUT_FILE_NAME"
    <> help "file where to save problem solution"
    <> value "output.txt")
  <*> strOption (
       long "evolOptions"
    <> short 'e'
    <> metavar "EVOL_OPTIONS_FILE_NAME"
    <> help "file with evolution options for model"
    <> value "options.txt")
  <*> strOption (
       long "fitnessModule"
    <> short 'f'
    <> metavar "FITNESS_FUNC_FILE_NAME"
    <> help "file with script where fitness function is defined"
    <> value "scripts/DefaultFitness.hs")
    
parseOptions :: IO Options
parseOptions = execParser opts 
  where
    opts = info (helper <*> options) (
         fullDesc
      <> progDesc "Starts genetic simulation with input from INPUT_FILE_NAME and will write final solution to OUTPUT_FILE_NAME. Evolution options are got from EVOL_OPTIONS_FILE_NAME and fitness function is got from script FITNESS_FUNC_FILE_NAME"
      <> header "radio-problem - solving 'radio-problem' with genetic algorithm" ) 