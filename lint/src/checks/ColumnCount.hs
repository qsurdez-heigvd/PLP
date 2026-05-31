module Checks.ColumnCount
    ( columnCountCheck
    ) where

import CSV (CSV(..), Field(..), Row(..))
import qualified CSV
import Check (Check(..))
import qualified Check
import Issue (Issue(..))
import qualified Issue

columnCountCheck :: Check
columnCountCheck = Check {
    key = "column-cont",
    run = checkColumnCount
}

checkColumnCount :: CSV -> [Issue]
checkColumnCount csv =
    let headerList = headers csv
        expectedCount = length headerList
        rowList = rows csv
        issues = concatMap (checkRow expectedCount) rowList
    in issues
  where
    checkRow :: Int -> Row -> [Issue]
    checkRow expected row =
        let actual = length row
        in if actual /= expected
           then case row of
               [] -> []  -- Empty row, skip (shouldn't happen in practice)
               (firstField:_) -> [createIssue expected actual firstField]
           else []

    createIssue :: Int -> Int -> Field -> Issue
    createIssue expected actual field = Issue
        { checkKey = key columnCountCheck
        , message = "Expected " ++ show expected ++ " columns, found " ++ show actual
        , Issue.filename = CSV.filename csv
        , Issue.location = CSV.location field
        }