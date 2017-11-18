-- TODO: limited import reduces binary size?
import Data.List.Split

{-
cut.hs imitates the UNIX cut tool.

```
cut -f|--fields <fields>
    -d|--delimiter <delim=TAB>
-}

-- type CutOp = (String -> [String])

type Delim = String
type Field = Int
type Row = String
type Column = String

data CutError = ContainsNoDelimiter
    deriving (Show, Eq)

-- TODO: better way of retrieving fields?
cut :: Delim -> [Field] -> Row -> Either CutError [String]
cut delim fields row
    | length columns == 1 = Left ContainsNoDelimiter
    | otherwise           = Right [columns !! field | field <- fields']
    where columns = splitOn delim row
          fields' = map (subtract 1) fields

-- translateFieldExpr :: String -> [Field]
