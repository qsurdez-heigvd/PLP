module Linter
    ( lint
    ) where

import CSV (CSV)
import Check (Check(..))
import Config (Config(..))
import Issue (Issue(..))

lint :: CSV -> Config -> [Issue]
lint csv config = 
    let checkList = checks config
        allIssues = concatMap (runCheck csv) checkList
    in allIssues
   where 
    runCheck :: CSV -> Check -> [Issue]
    runCheck csvData check = run check csvData

