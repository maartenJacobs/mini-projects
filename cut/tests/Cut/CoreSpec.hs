module Cut.CoreSpec (spec) where

import Test.Hspec
import Cut.Core

spec :: Spec
spec =
  describe "cutFields" $ do
    it "returns the unchanged row when it does not contain the delimiter" $
        cutFields ',' "," [] "row" `shouldBe` (Left $ ContainsNoDelimiter "row")
    it "returns the unchanged row when all fields are selected" $
        -- TODO: generate every ordering of the field numbers and every distribution of every ordering.
        sequence_ [
            cutFields ',' "," fs "f1,f2,f3" `shouldBe` Right "f1,f2,f3"
            | fs <- [[[1..3]], [[1..2], [3]]]]
    it "returns fields in order" $
        cutFields ',' "," [[3,2,1]] "f1,f2,f3" `shouldBe` Right "f1,f2,f3"
    it "returns fields only once" $
        cutFields ',' "," [[3], [3]] "f1,f2,f3" `shouldBe` Right "f3"
    it "joins the selected fields with the output delimiter" $
        cutFields ',' "__" [[1..3]] "f1,f2,f3" `shouldBe` Right "f1__f2__f3"