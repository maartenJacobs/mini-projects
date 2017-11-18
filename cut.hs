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

-- TODO: better way of retrieving fields?
cut :: Delim -> [Field] -> Row -> [String]
cut delim fields row = [columns !! field | field <- fields']
    where columns = splitOn delim row
          fields' = map (subtract 1) fields

-- translateFieldExpr :: String -> [Field]
