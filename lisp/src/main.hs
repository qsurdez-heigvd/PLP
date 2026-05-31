module Main where
  import System.Environment (getArgs, getProgName)
  import Tokenizer (tokenize, LexError(..))
  import Parser (parse, ParseError(..))
  import System.Exit (exitFailure)
  



  main :: IO()
  main = do
    args <- getArgs
    case args of 
      [filename] -> processFile filename
      _ -> do 
        printUsage
        exitFailure


  processFile :: FilePath -> IO()
  processFile filename = do
    content <- readFile filename

    case tokenize content of
      Left (LexError pos msg) -> do
        putStrLn $ "Lexical error at position " ++ show pos ++ ": " ++ msg
        exitFailure
      Right tokens -> do
        case parse tokens of 
          Left (ParseError pos msg) -> do
            putStrLn $ "Parse error at position " ++ show pos ++ ": " ++ msg
            exitFailure
          Right ast -> do
            print ast      


  printUsage :: IO()
  printUsage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " <filename>"
    exitFailure
