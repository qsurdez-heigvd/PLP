module CSV
    ( Location(..)
    , Field(..)
    , Row
    , CSV(..)
    , parseCSV
    ) where

-- | Represents a location in the CSV file
data Location = Location
    { line :: Int
    , column :: Int
    } deriving (Show, Eq)

-- | Represents a CSV field with its content and location
data Field = Field
    { content :: String
    , location :: Location
    } deriving (Show, Eq)

-- | Type alias for a row (list of entries)
type Row = [Field]

-- | Represents a parsed CSV file
data CSV = CSV
    { filename :: String
    , headers :: [Field]
    , rows :: [Row]
    } deriving (Show, Eq)

-- | Parse a CSV string into a CSV structure
-- Returns Nothing if the input is empty, otherwise returns Just CSV
-- Does not validate CSV structure
parseCSV :: String -> String -> Maybe CSV
parseCSV fname input
    | null (trimString input) = Nothing
    | otherwise = Just $ parseLines (lines input)
  where
    trimString = dropWhile (== '\n')
    
    parseLines [] = CSV fname [] []
    parseLines (firstLine : rest) =
        let headerEntries = parseRow firstLine 1
            rowEntries = zipWith (\lineNum row -> parseRow row lineNum) [2..] rest
        in CSV fname headerEntries rowEntries
    
    parseRow :: String -> Int -> Row
    parseRow rowStr lineNum = parseFields rowStr lineNum 1
    
    parseFields :: String -> Int -> Int -> Row
    parseFields "" _ _ = []
    parseFields str lineNum colNum =
        let (content, remainder) = span (/= ',') str
            field = Field content (Location lineNum colNum)
        in case remainder of
            "" -> [field]
            (',':rest) -> field : parseFields rest lineNum (colNum + length content + 1)
            _ -> [field]  -- Should not happen

