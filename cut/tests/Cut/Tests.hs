module Cut.Tests (tests) where

import Distribution.TestSuite
import qualified Distribution.TestSuite.QuickCheck as QuickCheckSuite
import Test.QuickCheck
import Cut.Core

tests :: IO [Test]
tests = return [
        QuickCheckSuite.testProperty "empty range selection equals empty row" prop_EmptyRangesEmpties
      , QuickCheckSuite.testProperty "row not containing delimiter returns error" prop_NoDelimiterFail
    ]

-- cutFields delim [] rowWithDelimiter = Right []
prop_EmptyRangesEmpties delimiter rowWithDelimiter = 
    delimiter `elem` rowWithDelimiter ==> 
        cutFields delimiter [] rowWithDelimiter == Right []
    where types = (delimiter::Char, rowWithDelimiter::String)

-- cutFields delim ranges rowWithoutDelimiter = Left $ ContainsNoDelimiter row
prop_NoDelimiterFail delimiter rowWithoutDelimiter = 
    not (delimiter `elem` rowWithoutDelimiter) ==> 
        cutFields delimiter [] rowWithoutDelimiter == Left (ContainsNoDelimiter rowWithoutDelimiter)
    where types = (delimiter::Char, rowWithoutDelimiter::String)
