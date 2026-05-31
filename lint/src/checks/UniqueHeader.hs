module Checks.UniqueHeader
    ( uniqueHeaderCheck
    ) where

import CSV (CSV(..), Field(..))
import qualified CSV
import Check (Check(..))
import qualified Check
import Issue (Issue(..))
import qualified Issue
import Data.List (group, sort)

-- | Check that flags headers whose names are duplicates
uniqueHeaderCheck :: Check
uniqueHeaderCheck = Check
    { key = "unique-header"
    , run = checkUniqueHeader
    }

checkUniqueHeader :: CSV -> [Issue]
checkUniqueHeader csv =
    let headerList = headers csv
        duplicates = findDuplicates headerList
        issues = concatMap createIssues duplicates
    in issues
  where
    -- Find entries with duplicate content
    findDuplicates :: [Field] -> [[Field]]
    findDuplicates entries =
        let grouped = groupByContent entries
            duplicateGroups = filter (\g -> length g > 1) grouped
        in duplicateGroups
    
    -- Group entries by their content
    groupByContent :: [Field] -> [[Field]]
    groupByContent entries =
        let sorted = sortByContent entries
        in groupBy' (\e1 e2 -> content e1 == content e2) sorted
    
    sortByContent :: [Field] -> [Field]
    sortByContent = sortBy' (\e1 e2 -> compare (content e1) (content e2))
    
    -- Custom groupBy since we need to import it
    groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
    groupBy' _ [] = []
    groupBy' eq (x:xs) = (x : ys) : groupBy' eq zs
      where
        (ys, zs) = span (eq x) xs
    
    -- Custom sortBy
    sortBy' :: (a -> a -> Ordering) -> [a] -> [a]
    sortBy' _ [] = []
    sortBy' cmp (x:xs) =
        let smaller = sortBy' cmp [a | a <- xs, cmp a x == LT]
            greater = sortBy' cmp [a | a <- xs, cmp a x /= LT]
        in smaller ++ [x] ++ greater
    
    createIssues :: [Field] -> [Issue]
    createIssues fields = map createIssue fields
    
    createIssue :: Field -> Issue
    createIssue field = Issue
        { checkKey = key uniqueHeaderCheck
        , message = "Duplicate header name: " ++ content field
        , Issue.filename = CSV.filename csv
        , Issue.location = CSV.location field
        }

