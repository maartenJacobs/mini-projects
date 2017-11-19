import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.Char (ord)
import Text.Parsec
import qualified Options.Applicative as Options

{-
cut.hs imitates the UNIX cut tool.

```
cut -f|--fields <fields>
    -d|--delimiter <delim=TAB>
-}

type Delim = Char
type Row = String
type Column = String
type Range = [Int]

data CutError = ContainsNoDelimiter Row
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

data CutOptions = CutOptions
    { file      :: String
    , fields    :: String
    , delimiter :: String }

cutOptions :: Options.Parser CutOptions
cutOptions = CutOptions
            <$> Options.argument Options.str (Options.metavar "FILE")
            <*> Options.strOption
                (  Options.long "fields"
                <> Options.short 'f'
                <> Options.metavar "LIST"
                <> Options.help "select only these fields")
            <*> Options.strOption
                (  Options.long "delimiter"
                <> Options.short 'd'
                <> Options.metavar "DELIM"
                <> Options.value "\t"
                <> Options.help "use DELIM instead of TAB for field delimiter")

printCutResult :: Either CutError Row -> IO ()
printCutResult (Left (ContainsNoDelimiter row)) = putStrLn row
printCutResult (Right row)                      = putStrLn row

executeCut :: CutOptions -> IO ()
executeCut (CutOptions file fields (delim:[])) =
    do content <- readFile file
       let rows' = map (cut delim (translateFieldExpr fields)) $ lines content
       mapM_ printCutResult rows'
executeCut _ = putStrLn "No options provided!!!"

main :: IO ()
main = executeCut =<< Options.execParser opts
    where opts = Options.info (cutOptions Options.<**> Options.helper)
            (Options.fullDesc
            <> Options.progDesc "Print selected parts of lines from each FILE to standard output."
            <> Options.header "hello - a test for optparse-applicative")
