import Test.Hspec
import Test.QuickCheck as QC hiding ((===))
import Test.Hspec.QuickCheck
import Test.Hspec.LeanCheck as LC
import Test.Hspec.SmallCheck as SC
import Test.LeanCheck.Utils.Operators

import Safe

import qualified Data.IntMap as IntMap

import Aoc2020Day15

instance Listable a => Listable (NonEmptyList a) where
  list = (map NonEmpty . tail) list

instance (Listable a, Ord a, Num a) => Listable (NonNegative a) where
  list = (map NonNegative . filter (> 0)) list


main :: IO ()
main = hspec $ do
  describe "shoutInfOptAt :: [Int] -> Int -> Int" $ do
    context "Given Testcases" $ do
      it "shoutInfOptAt [0,3,6] 0 ->> 0" $
        shoutInfOptAt [0,3,6] 0 `shouldBe` 0
      it "shoutInfOptAt [0,3,6] 1 ->> 3" $
        shoutInfOptAt [0,3,6] 1 `shouldBe` 3
      it "shoutInfOptAt [0,3,6] 2 ->> 6" $
        shoutInfOptAt [0,3,6] 2 `shouldBe` 6
      it "shoutInfOptAt [0,3,6] 3 ->> 0" $
        shoutInfOptAt [0,3,6] 3 `shouldBe` 0
      it "shoutInfOptAt [0,3,6] 4 ->> 3" $
        shoutInfOptAt [0,3,6] 4 `shouldBe` 3
      it "shoutInfOptAt [0,3,6] 5 ->> 3" $
        shoutInfOptAt [0,3,6] 5 `shouldBe` 3
      it "shoutInfOptAt [0,3,6] 6 ->> 1" $
        shoutInfOptAt [0,3,6] 6 `shouldBe` 1
      it "shoutInfOptAt [0,3,6] 7 ->> 0" $
        shoutInfOptAt [0,3,6] 7 `shouldBe` 0
      it "shoutInfOptAt [0,3,6] 8 ->> 4" $
        shoutInfOptAt [0,3,6] 8 `shouldBe` 4
      it "shoutInfOptAt [0,3,6] 9 ->> 0" $
        shoutInfOptAt [0,3,6] 9 `shouldBe` 0
-- takes very long
--      it "shoutInfOptAt [0,3,6] ((30000000)-1) ->> 175594" $
--        shoutInfOptAt [0,3,6] ((30000000)-1) `shouldBe` 175594
    context "Derived Properties" $ do
      it "should equal non-opt variant: LeanCheck" $
        LC.propertyFor 25000 $ propShouldEqualNonOpt
      modifyMaxSuccess (const 1000) $ it "should equal non-opt variant: QuickCheck" $
        QC.property $ propShouldEqualNonOpt
  describe "shoutNextOpt :: Int -> Int -> IntMap Int -> Int" $ do
    context "Given Testcases" $ do
      it "shoutNextOpt 3 6 (IntMap.fromList [(0,0),(3,1)]) ->> 0" $
        shoutNextOpt 3 6 (IntMap.fromList [(0,0),(3,1)]) `shouldBe` 0
      it "shoutNextOpt 4 0 (IntMap.fromList [(0,0),(3,1),(6,2)]) ->> 3" $
        shoutNextOpt 4 0 (IntMap.fromList [(0,0),(3,1),(6,2)]) `shouldBe` 3
      it "shoutNextOpt 5 3 (IntMap.fromList [(0,3),(3,1),(6,2)]) ->> 3" $
        shoutNextOpt 5 3 (IntMap.fromList [(0,3),(3,1),(6,2)]) `shouldBe` 3
      it "shoutNextOpt 6 3 (IntMap.fromList [(0,3),(3,4),(6,2)]) ->> 1" $
        shoutNextOpt 6 3 (IntMap.fromList [(0,3),(3,4),(6,2)]) `shouldBe` 1
  describe "shoutInf :: [Int] -> [Int]" $ do
    context "Given Testcases" $ do
      it "take 10 . shoutInf [0,3,6] ->> [0,3,6,0,3,3,1,0,4,0]" $
        (take 10 . shoutInf) [0,3,6] `shouldBe` [0,3,6,0,3,3,1,0,4,0]
      it "shoutInf [1,3,2] !! 2019 ->> 1" $
        shoutInf [1,3,2] `at` 2019 `shouldBe` 1
      it "shoutInf [2,1,3] !! 2019 ->> 10" $
        shoutInf [2,1,3] `at` 2019 `shouldBe` 10
      it "shoutInf [1,2,3] !! 2019 ->> 27" $
        shoutInf [1,2,3] `at` 2019 `shouldBe` 27
      it "shoutInf [2,3,1] !! 2019 ->> 78" $
        shoutInf [2,3,1] `at` 2019 `shouldBe` 78
      it "shoutInf [3,2,1] !! 2019 ->> 438" $
        shoutInf [3,2,1] `at` 2019 `shouldBe` 438
      it "shoutInf [3,1,2] !! 2019 ->> 1836" $
        shoutInf [3,1,2] `at` 2019 `shouldBe` 1836
  describe "shoutNext :: [Int] -> Int" $ do
    context "Given Testcases" $ do
      it "shoutNext [0,3,6] ->> 0" $
        shoutNext [0,3,6] `shouldBe` 0
      it "shoutNext [0,3,6,0] ->> 3" $
        shoutNext [0,3,6,0] `shouldBe` 3
      it "shoutNext [0,3,6,0,3] ->> 3" $
        shoutNext [0,3,6,0,3] `shouldBe` 3
      it "shoutNext [0,3,6,0,3,3] ->> 1" $
        shoutNext [0,3,6,0,3,3] `shouldBe` 1
      it "shoutNext [0,3,6,0,3,3,1] ->> 0" $
        shoutNext [0,3,6,0,3,3,1] `shouldBe` 0
      it "shoutNext [0,3,6,0,3,3,1,0] ->> 4" $
        shoutNext [0,3,6,0,3,3,1,0] `shouldBe` 4
      it "shoutNext [0,3,6,0,3,3,1,0,4] ->> 0" $
        shoutNext [0,3,6,0,3,3,1,0,4] `shouldBe` 0

propShouldEqualNonOpt :: NonEmptyList (NonNegative Int) -> NonNegative Int -> Bool
propShouldEqualNonOpt (NonEmpty ls) (NonNegative index) = ((shoutInf ls') `at` index) == (shoutInfOptAt ls' index)
  where
    ls' = map getNonNegative ls
