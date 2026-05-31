module Tokenizer
  (
    Token(..),
    TokenType(..),
    tokenize,
    LexError(..)
  ) where

import Data.Char (isDigit, isSpace) 
import AST (isSymbolChar, isSymbolStart)  

-- | Token with position information
data Token = Token
  { tokenType :: TokenType
  , tokenPos  :: Int  -- Offset from start of file
  } deriving (Show, Eq)

-- | Token types based on Lisp grammar
data TokenType
  = TSymbol String
  | TNumber Int
  | TString String
  | TLeftParen
  | TRightParen
  | TEOF
  deriving (Show, Eq)

-- | Lexical error with position
data LexError = LexError
  { lexErrorPos :: Int
  , lexErrorMsg :: String
  } deriving (Show, Eq)

-- | Tokenize a source string into tokens or return a lexical error
tokenize :: String -> Either LexError [Token]
tokenize input = runTokenizer input 0

-- | Internal tokenizer state
runTokenizer :: String -> Int -> Either LexError [Token]
runTokenizer [] pos = Right [Token TEOF pos]
runTokenizer input@(c:cs) pos
  | isSpace c = runTokenizer cs (pos + 1)
  | c == '('  = do
      rest <- runTokenizer cs (pos + 1)
      return $ Token TLeftParen pos : rest
  | c == ')'  = do
      rest <- runTokenizer cs (pos + 1)
      return $ Token TRightParen pos : rest
  | c == '"'  = tokenizeString cs (pos + 1) pos []
  | isDigit c = tokenizeNumber input pos
  | isSymbolStart c = tokenizeSymbol input pos
  | otherwise = Left $ LexError pos $ "Unexpected character '" ++ [c] ++ "' at position " ++ show pos

-- | Tokenize a number
tokenizeNumber :: String -> Int -> Either LexError [Token]
tokenizeNumber input pos =
  let (digits, rest) = span isDigit input
      num = read digits :: Int
      newPos = pos + length digits
  in do
    tokens <- runTokenizer rest newPos
    return $ Token (TNumber num) pos : tokens

-- | Tokenize a symbol
tokenizeSymbol :: String -> Int -> Either LexError [Token]
tokenizeSymbol input pos =
  let (symbolChars, rest) = span isSymbolChar input
      newPos = pos + length symbolChars
  in do
    tokens <- runTokenizer rest newPos
    return $ Token (TSymbol symbolChars) pos : tokens

-- | Tokenize a string literal
tokenizeString :: String -> Int -> Int -> String -> Either LexError [Token]
tokenizeString [] currentPos startPos _ =
  Left $ LexError startPos $ "Unterminated string at position " ++ show startPos
tokenizeString (c:cs) currentPos startPos acc
  | c == '"'  = do
      tokens <- runTokenizer cs (currentPos + 1)
      return $ Token (TString (reverse acc)) startPos : tokens
  | otherwise = tokenizeString cs (currentPos + 1) startPos (c:acc)
