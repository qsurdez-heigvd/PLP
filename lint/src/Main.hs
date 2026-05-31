module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import Linter (lint)
import Config (Config(..), defaultConfig, parseConfig)
import CSV (parseCSV)
import Issue (Issue(..))

main :: IO ()
main = do
  args <- getArgs
  case args of
    []            -> printUsageAndExit
    ("-h":_)      -> printHelpAndExit
    ("--help":_)  -> printHelpAndExit
    ("init":_)    -> initConfig
    files         -> lintFiles files

printUsageAndExit :: IO()
printUsageAndExit = do
  putStrLn "Error: no files specified"
  printHelp
  exitFailure

printHelpAndExit :: IO()
printHelpAndExit = do
  printHelp
  exitSuccess

initConfig :: IO()
initConfig = do
  let configFile = "csvlint.config.txt"
  exists <- doesFileExist configFile
  if exists
    then do
      putStrLn ("Error: " ++ configFile ++ " already exists.")
      exitFailure
    else do
      writeFile configFile ""
      putStrLn ("Created " ++ configFile)

loadConfig :: IO Config
loadConfig = do
  let configFile = "csvlint.config.txt"
  exists <- doesFileExist configFile
  if not exists
    then return defaultConfig
    else do
      content <- readFile configFile
      case parseConfig content of
        Left err -> do
          putStrLn ("Warning: failed to parse config file: " ++ err)
          exitFailure
        Right config -> return config


lintFiles :: [FilePath] -> IO()
lintFiles files = do
  config <- loadConfig
  results <- mapM (lintFile config) files
  let allIssues = concat results
  if null allIssues
    then do
      putStrLn "No issues found"
      exitSuccess
    else do
      mapM_ print allIssues
      exitFailure

lintFile :: Config -> FilePath -> IO [Issue]
lintFile config filePath = do
  exists <- doesFileExist filePath
  if not exists
    then do
      putStrLn ("Error: file not found " ++ filePath)
      exitFailure
    else do
      content <- readFile filePath
      case parseCSV filePath content of
        Nothing -> do
          putStrLn ("Error: failed to parse CSV file: " ++ filePath)
          exitFailure
        Just csv -> do
          let issues = lint csv config
          return issues




printHelp :: IO ()
printHelp = do
    putStrLn "csvlint - A CSV file linter"
    putStrLn ""
    putStrLn "Usage:"
    putStrLn "  csvlint file.csv [file2.csv ...]   - Lint one or more CSV files"
    putStrLn "  csvlint init                       - Create a default config file"
    putStrLn "  csvlint -h, --help                 - Show this help message"
    putStrLn ""
    putStrLn "Configuration:"
    putStrLn "  Place a csvlint.config.txt file in the current directory to configure checks."
    putStrLn "  Each line should contain one check key."
    putStrLn ""
    putStrLn "All checks are run by default if no configuration file is present."
    putStrLn ""
    putStrLn "Available checks:"
    putStrLn "  column-count   - Validates consistent column count across rows"
    putStrLn "  empty-header   - Checks for empty header fields"
    putStrLn "  unique-header  - Ensures header names are unique"
