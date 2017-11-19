import Data.List.Split (splitOn)
import Text.Parsec

{-
cut.hs imitates the UNIX cut tool.

```
cut -f|--fields <fields>
    -d|--delimiter <delim=TAB>
-}

type Delim = String
type Row = String
type Column = String
type Range = [Int]

data CutError = ContainsNoDelimiter
    deriving (Show, Eq)

-- TODO: better way of retrieving fields?
cut :: Delim -> [Range] -> Row -> Either CutError [String]
cut delim rs row
    | length columns == 1 = Left ContainsNoDelimiter
    | otherwise           = Right . concat $ map (extractFields columns) normalisedRanges
    where columns = splitOn delim row
          columnCount = length columns
          normalisedRanges = map (limitRange columnCount . map (subtract 1)) rs
          extractFields cols range = [cols !! field | field <- range]

limitRange :: Int -> Range -> Range
limitRange max = takeWhile (< max)

-- Translates field expressions into the requested fields.
-- Field expressions are 1 or more ranges separated by commas.
-- A range is denoted by 2 numbers separated by a hyphen.
-- A range can also be open on one end, but not both. An open range has
-- one of the 2 boundaries unspecified.
translateFieldExpr :: String -> [Range]
translateFieldExpr expr = case parse ranges expr expr of
                            (Left _)   -> error "invalid field list"
                            (Right rs) -> rs

range :: Parsec String st Range
range = try closedRange <|> try halfOpenRange <|> onlyDigits

digits :: Parsec String st Int
digits = read <$> (many1 $ oneOf ['0'..'9'])

onlyDigits :: Parsec String st Range
onlyDigits = (: []) <$> digits

closedRange :: Parsec String st Range
closedRange = do start <- digits
                 char '-'
                 end <- digits
                 return [start..end]

halfOpenRange :: Parsec String st Range
halfOpenRange = openStart <|> openEnd

openEnd :: Parsec String st Range
openEnd = do char '-'
             end <- digits
             return [1..end]

openStart :: Parsec String st Range
openStart = do start <- digits
               char '-'
               return [start..]

ranges :: Parsec String st [Range]
ranges = range `sepBy` (char ',')
