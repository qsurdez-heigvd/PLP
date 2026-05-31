module Issue
    ( Issue(..)
    ) where

import CSV (Location(..))

-- | Represents a linting issue found in a CSV
data Issue = Issue
    { checkKey :: String
    , message  :: String
    , filename :: String
    , location :: Location
    } deriving (Eq)

-- | Custom Show instance for Issue
-- Format: "<filename> <line>:<column> <message> (<check-key>)"
instance Show Issue where
    show issue = filename issue ++ " "
               ++ show (line $ location issue) ++ ":"
               ++ show (column $ location issue) ++ " "
               ++ message issue ++ " ("
               ++ checkKey issue ++ ")"

