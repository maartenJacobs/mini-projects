module Cut.Core
    ( cut
    , CutError(..)
    , Range
    , Row)
where

import Data.List.Split (splitOn)
import Data.List (intercalate)

type Delim = Char
type Row = String
type Column = String
type Range = [Int]

newtype CutError = ContainsNoDelimiter Row
    deriving (Show, Eq)

-- TODO: better way of retrieving fields?
cut :: Delim -> [Range] -> Row -> Either CutError Row
cut delim rs row
    | length columns == 1 = Left $ ContainsNoDelimiter row
    | otherwise           = Right . intercalate [delim] . concat $ map (extractFields columns) normalisedRanges
    where columns = splitOn [delim] row
          columnCount = length columns
          normalisedRanges = map (limitRange columnCount . map (subtract 1)) rs
          extractFields cols range = [cols !! field | field <- range]

limitRange :: Int -> Range -> Range
limitRange max = takeWhile (< max)
