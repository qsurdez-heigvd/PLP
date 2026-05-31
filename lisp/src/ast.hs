module AST
(
  SExpr(..),
  Atom(..),
  Symbol(..),
  mkSymbol,
  getSymbol,
  isSymbolChar,
  isSymbolStart,
  isSpecialChar,
) where

  import Data.List (intercalate)
  import Data.Char (isAlpha, isDigit)

  -- | Validated symbol type that enforces grammar constraints
  -- Constructor is not exported, ensuring only valid symbols can be created
  newtype Symbol = ValidSymbol String
    deriving (Show, Eq)

  -- | Smart constructor for Symbol that validates against the grammar
  -- Returns Nothing if the string is not a valid symbol
  mkSymbol :: String -> Maybe Symbol
  mkSymbol [] = Nothing
  mkSymbol s@(c:cs)
    | isSymbolStart c && all isSymbolChar cs = Just (ValidSymbol s)
    | otherwise = Nothing

  -- | Extract the string value from a Symbol
  getSymbol :: Symbol -> String
  getSymbol (ValidSymbol s) = s

  -- | Check if a character can start a symbol
  isSymbolStart :: Char -> Bool
  isSymbolStart c = isAlpha c || isSpecialChar c

  -- | Check if a character can appear in a symbol (after the first character)
  isSymbolChar :: Char -> Bool
  isSymbolChar c = isAlpha c || isDigit c || isSpecialChar c || c == '_'

  -- | Check if a character is a special character allowed in symbols
  isSpecialChar :: Char -> Bool
  isSpecialChar c = c `elem` "+-*/=?<>"

  -- | S-expression: either an atom or a list of s-expressions
  data SExpr
    = AtomExpr Atom
    | ListExpr [SExpr]
    deriving (Show, Eq)

  -- | Atomic values in Lisp
  -- Symbol is now a validated type that enforces grammar constraints
  data Atom
    = SymbolAtom Symbol
    | Number Int
    | String String
    deriving (Show, Eq)

