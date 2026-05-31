module Check
    ( Check(..)
    , CheckRunner
    ) where

import CSV (CSV)
import Issue (Issue)

-- | Type alias for a check runner
type CheckRunner = CSV -> [Issue]

-- | Represents a linting check
data Check = Check
    { key :: String
    , run :: CheckRunner
    }

