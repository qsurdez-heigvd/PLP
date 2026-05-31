module Parser 
  (
    parse,
    ParseError(..)
  ) where

  import AST (SExpr(..), Atom(..), mkSymbol)
  import Tokenizer (Token(..), TokenType(..))


  data ParseError = ParseError
    {
      parseErrorPos :: Int,
      parseErrorMsg :: String
    } deriving (Show, Eq)
  
  type Parser a = [Token] -> Either ParseError (a, [Token])

  parse :: [Token] -> Either ParseError SExpr
  parse tokens = case parseSExpr tokens of
    Left err -> Left err
    Right (expr, rest) -> case rest of
      [Token TEOF _ ] -> Right expr
      (Token _ pos : _) -> Left $ ParseError pos $ "Unexpected tokens after expression at position " ++ show pos
      [] -> Right expr

  parseSExpr :: Parser SExpr
  parseSExpr [] = Left $ ParseError 0 "Unexpected end of input"
  parseSExpr (token:rest) = case tokenType token of 
    TLeftParen -> parseList (token:rest)
    TSymbol _ -> parseAtom (token:rest)
    TNumber _ -> parseAtom (token:rest)
    TString _ -> parseAtom (token:rest)
    TRightParen -> Left $ ParseError (tokenPos token) $ "Unexpected ')' at position " ++ show (tokenPos token)
    TEOF -> Left $ ParseError (tokenPos token) "Unexpected end of input"

  parseAtom :: Parser SExpr
  parseAtom [] = Left $ ParseError 0 "Unexpected end of input"
  parseAtom (token:rest) = case tokenType token of
    TSymbol s -> case mkSymbol s of
      Just sym -> Right (AtomExpr (SymbolAtom sym), rest)
      Nothing -> Left $ ParseError (tokenPos token) $ "Invalid symbol '" ++ s ++ "' at position " ++ show (tokenPos token)
    TNumber n -> Right (AtomExpr (Number n), rest)
    TString s -> Right (AtomExpr (String s), rest)
    _ -> Left $ ParseError (tokenPos token) $ "Expected atom at position " ++ show (tokenPos token)

  parseList :: Parser SExpr
  parseList [] = Left $ ParseError 0 "Unexpected end of input"
  parseList (token:rest) = case tokenType token of 
    TLeftParen -> parseListElements rest (tokenPos token) []
    _ -> Left $ ParseError (tokenPos token) $ "Expected '(' at position " ++ show (tokenPos token)
  
  parseListElements :: [Token] -> Int -> [SExpr] -> Either ParseError (SExpr, [Token])
  parseListElements [] startPos _ = 
    Left $ ParseError startPos $ "Unclosed list starting at position " ++ show startPos
  parseListElements (token:rest) startPos acc = case tokenType token of 
    TRightParen -> Right (ListExpr (reverse acc), rest)
    TEOF -> Left $ ParseError startPos $ "Unclosed list starting at position " ++ show startPos
    _ -> case parseSExpr (token:rest) of 
      Left err -> Left err
      Right (expr, remaining) -> parseListElements remaining startPos (expr:acc)

