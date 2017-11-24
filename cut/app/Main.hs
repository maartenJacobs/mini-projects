module Main where

import qualified Options.Applicative as Options
import Data.Semigroup ((<>))
import qualified Cut.Parser as Parser
import qualified Cut.Core as Cut

{-
cut.hs imitates the UNIX cut tool.

TODO:
    - Create option parser for delimiter that accepts a Char. Read up on `optparse-applicative`.
    - Figure out how to list dependencies using `cabal`.
    - Split out program into modules: core functionality (cut and field exprs) and command (options parser)
        would be better as modules. Currently there are collisions between the option parser and the
        field expression parse.
    - Write tests. Quickcheck seems quite popular.
    - Test performance against UNIX cut. Expect 100x slowdown. Try not to cry if more than that.
    - Attempt to parallelise cutting of each line. Test performance again.
    - (Optional) Add more options.

```
cut -f|--fields <fields>
    -d|--delimiter <delim=TAB>
-}

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

printCutResult :: Either Cut.CutError Cut.Row -> IO ()
printCutResult (Left (Cut.ContainsNoDelimiter row)) = putStrLn row
printCutResult (Right row)                      = putStrLn row

executeCut :: CutOptions -> IO ()
executeCut (CutOptions file fields [delim]) =
    do content <- readFile file
       let rows' = map (Cut.cutFields delim (Parser.translateFieldExpr fields)) $ lines content
       mapM_ printCutResult rows'
executeCut _ = putStrLn "No options provided!!!"

main :: IO ()
main = executeCut =<< Options.execParser opts
    where opts = Options.info (cutOptions Options.<**> Options.helper)
            (Options.fullDesc
            <> Options.progDesc "Print selected parts of lines from each FILE to standard output."
            <> Options.header "Cut")
