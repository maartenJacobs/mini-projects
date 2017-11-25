module Cut.Core
    ( cutFields
    , CutError(..)
    , Range
    , Row)
where

import Data.List.Split (splitOn)
import Data.List (intercalate, sort)

type Delim = Char
type Row = String
type Column = String
type Range = [Int]
type Field = String

newtype CutError = ContainsNoDelimiter Row
    deriving (Show, Eq)

-- TODO: better way of retrieving fields?
cutFields :: Delim -> [Range] -> Row -> Either CutError Row
cutFields delim rs row
    | delim `elem` row  = Right . joinWithDelim $ fieldsFromRange fields normalisedRange
    | otherwise         = Left $ ContainsNoDelimiter row
    where fields = splitOn [delim] row
          joinWithDelim = intercalate [delim]
          normalisedRange = normaliseRanges rs (length fields)

fieldsFromRange :: [Field] -> Range -> [Field]
fieldsFromRange fs range = [fs !! idx | idx <- range]

normaliseRanges :: [Range] -> Int -> Range
normaliseRanges rs maxLen = uniq . sort . concat $ map (limitRange maxLen . map zeroIndex) rs
    where zeroIndex = subtract 1
          limitRange max = takeWhile (< max)

uniq :: Range -> Range
uniq []     = []
uniq (f:fs) = f : uniq' fs f
    where uniq' [] _ = []
          uniq' (x:xs) p | x == p    = uniq' xs p
                         | otherwise = x : uniq' xs x
