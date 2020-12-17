import Test.Hspec
import Test.QuickCheck as QC hiding ((===))
import Test.Hspec.QuickCheck
import Test.Hspec.LeanCheck as LC
import Test.Hspec.SmallCheck as SC
import Test.LeanCheck.Utils.Operators

import Safe
import Aoc2020Day17

import Data.Array

testDimension = nullDimensionOf 3 3 1 // [((-1, 0, 0), True),
                                          ((0, 1, 0), True),
                                          ((1, -1, 0), True), ((1, 0, 0), True), ((1, 1, 0), True)]

progress1 = nullDimensionOf 3 3 3 // [
  ((-1,-1,-1), True), ((0,1,-1), True), ((1,0,-1), True),
  ((-1,-1,0), True), ((-1,1,0), True), ((0,0,0), True), ((0,1,0), True), ((1,0,0), True),
  ((-1,-1,1), True), ((0,1,1), True), ((1,0,1), True)]

--progress2 = nullDimensionOf

main :: IO ()
main = hspec $ do
  describe "progressDimension progressDimension :: PocketDimension -> PocketDimension" $ do
    context "Given testcases" $ do
      it "progressDimension testDimension ->> progress1" $
        progressDimension testDimension `shouldBe` progress1
