module Checks.EmptyHeader
    ( emptyHeaderCheck
    ) where

import CSV (CSV(..), Field(..))
import qualified CSV
import Check (Check(..))
import qualified Check
import Issue (Issue(..))
import qualified Issue
import Data.Char (isSpace)

-- | Check that flags headers that are empty
emptyHeaderCheck :: Check
emptyHeaderCheck = Check
    { key = "empty-header"
    , run = checkEmptyHeader
    }

checkEmptyHeader :: CSV -> [Issue]
checkEmptyHeader csv =
    let headerList = headers csv
        emptyHeaders = filter (isEmpty . content) headerList
        issues = map createIssue emptyHeaders
    in issues
  where
    isEmpty :: String -> Bool
    isEmpty str = all isSpace str
    
    createIssue :: Field -> Issue
    createIssue field = Issue
        { checkKey = key emptyHeaderCheck
        , message = "Header is empty"
        , Issue.filename = CSV.filename csv
        , Issue.location = CSV.location field
        }
