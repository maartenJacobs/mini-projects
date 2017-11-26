module Main where

import qualified Options.Applicative as Options
import Data.Semigroup ((<>))
import System.IO (Handle, hGetContents, openFile, IOMode(ReadMode), stdin)
import Control.Applicative ((<|>))
import qualified Cut.Parser as Parser
import qualified Cut.Core as Cut

{-
cut.hs imitates the UNIX cut tool.

TODO:
    - Create option parser for delimiter that accepts a Char. Read up on `optparse-applicative`.
    - Test performance against UNIX cut. Expect 100x slowdown.
        `yes | ./dist/build/cut/cut -f 1-3 -d, | pv > /dev/null` reports 5.2MiB/second.
        `yes | cut -f 1-3 -d, | pv > /dev/null` reports 175MiB/second.
    - Attempt to parallelise cutting of each line. Test performance again.
    - (Optional) Add more options.

```
cut -f|--fields <fields>
    -d|--delimiter <delim=TAB>
    [FILE|standard input]
-}

data Input = FileInput FilePath | StdInput

data OutputDelimiter = OutDelim String | UseInDelim

data CutOptions = CutOptions
                        { file          :: Input
                        , fields        :: String
                        , delimiter     :: String
                        , onlyDelimited :: Bool
                        , outDelimiter  :: OutputDelimiter }

fileInput :: Options.Parser Input
fileInput = FileInput <$> Options.argument Options.str (Options.metavar "FILE")

stdInput :: Options.Parser Input
stdInput = pure StdInput

outputDelimiter :: Options.Parser OutputDelimiter
outputDelimiter   = OutDelim
                        <$> Options.strOption
                            (  Options.long "output-delimiter"
                            <> Options.metavar "STRING"
                            <> Options.help "use STRING as the output delimiter. The default is to use the input delimiter")
                    <|> pure UseInDelim

cutOptions :: Options.Parser CutOptions
cutOptions = CutOptions
        <$> (fileInput <|> stdInput)
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
        <*> Options.switch
            (  Options.long "only-delimited"
            <> Options.short 's'
            <> Options.help "do not print lines not containing delimiters")
        <*> outputDelimiter

printCutResult :: Bool -> Either Cut.CutError Cut.Row -> IO ()
printCutResult _ (Right row)                              = putStrLn row
printCutResult False (Left (Cut.ContainsNoDelimiter row)) = putStrLn row
printCutResult _ _                                        = return ()

getOutputDelimiter :: Char -> OutputDelimiter -> String
getOutputDelimiter inDelim UseInDelim = [inDelim]
getOutputDelimiter _ (OutDelim delim) = delim

executeCut :: CutOptions -> IO ()
executeCut (CutOptions input fields [delim] onlyDelimited outDelimiter) =
    do (closeOnEnd, handle) <- inputToHandle input
       content <- hGetContents handle
       let ranges = Parser.translateFieldExpr fields
       let outDelimiter' = getOutputDelimiter delim outDelimiter
       let rows' = map (Cut.cutFields delim outDelimiter' ranges) $ lines content
       mapM_ (printCutResult onlyDelimited) rows'
executeCut _ = putStrLn "No options provided!!!"

inputToHandle :: Input -> IO (Bool, Handle)
inputToHandle (FileInput fpath) = do handle <- openFile fpath ReadMode
                                     return (True, handle)
inputToHandle StdInput          = return (False, stdin)

main :: IO ()
main = executeCut =<< Options.execParser opts
    where opts = Options.info (cutOptions Options.<**> Options.helper)
            (Options.fullDesc
            <> Options.progDesc "Print selected parts of lines from each FILE to standard output."
            <> Options.header "Cut")
