module Config
    ( Config(..)
    , defaultConfig
    , parseConfig
    ) where

import Check (Check(..))
import Checks.ColumnCount (columnCountCheck)
import Checks.EmptyHeader (emptyHeaderCheck)
import Checks.UniqueHeader (uniqueHeaderCheck)
import Data.Char (isSpace)

-- | Configuration for the linter
newtype Config = Config
    { checks :: [Check]
    } deriving ()

defaultConfig :: Config
defaultConfig = Config {
    checks = [
        columnCountCheck,
        uniqueHeaderCheck,
        emptyHeaderCheck
    ]
}

parseConfig :: String -> Either String Config
parseConfig input =
    let configLines     = lines input
        trimmedLines    = map trimString configLines
        nonEmptyLines   = filter (not . null) trimmedLines
    in case mapM lookupCheck nonEmptyLines of
        Nothing -> Left "Invalid key check found. Valid keys: column-count, empty-header, unique-header"
        Just selectedCheck -> Right $ Config { checks = selectedCheck }
    where
        trimString :: String -> String
        trimString = removeLeading . removeTrailing
            where
                removeLeading = dropWhile isSpace
                removeTrailing = reverse . removeLeading . reverse

        lookupCheck :: String -> Maybe Check
        lookupCheck checkKey = case checkKey of
            "column-count"  -> Just columnCountCheck
            "empty-header"  -> Just emptyHeaderCheck
            "unique-header" -> Just uniqueHeaderCheck
            _               -> Nothing

        -- Check wether or not it needs to be redefined ?
        mapM :: (a -> Maybe b) -> [a] -> Maybe [b]
        mapM _ [] = Just []
        mapM f (x:xs) = case f x of
            Nothing -> Nothing
            Just y  -> case mapM f xs of
                Nothing -> Nothing
                Just ys -> Just (y:ys)

