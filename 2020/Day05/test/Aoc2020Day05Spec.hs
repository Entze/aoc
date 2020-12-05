{-# LANGUAGE FlexibleInstances #-}
import Aoc2020Day05

import Test.Hspec
import Test.QuickCheck as QC
import Test.Hspec.QuickCheck
import Test.Hspec.LeanCheck as LC

import Data.Maybe
import Data.List

newtype ValidBoardingPass = ValidBoardingPass BoardingPass deriving (Eq, Show)

getBoardingPass :: ValidBoardingPass -> BoardingPass
getBoardingPass (ValidBoardingPass p) = p

instance Listable ValidBoardingPass where
  list = [ ValidBoardingPass (r1:r2:r3:r4:r5:r6:r7:s1:s2:s3:[]) | r1 <- "FB", r2 <- "FB", r3 <- "FB", r4 <- "FB", r5 <- "FB", r6 <- "FB", r7 <- "FB", s1 <- "LR", s2 <- "LR", s3 <- "LR"]

instance Arbitrary ValidBoardingPass where
  arbitrary = do
    rows <- (vectorOf 7 . elements) "FB"
    seats <- (vectorOf 3 . elements) "LR"
    return (ValidBoardingPass (rows ++ seats))
  shrink (ValidBoardingPass "FFFFFFFLLL") = []
  shrink (ValidBoardingPass s) = (map ValidBoardingPass . nub . delete s) [r++c',r'++c, r'++c']
    where
      (r,c) = splitAt 7 s
      r' = predBoardingPass r
      c' = predBoardingPass c

predBoardingPass "LLL" = "LLL"
predBoardingPass "FFFFFFF" = "FFFFFFFF"
predBoardingPass p = (reverse . predBoardingPass' . reverse) p
  where
    predBoardingPass' ('B':l) = 'F':l
    predBoardingPass' ('F':l) = 'B':(predBoardingPass' l)
    predBoardingPass' ('R':l) = 'L':l
    predBoardingPass' ('L':l) = 'R':(predBoardingPass' l)
    predBoardingPass' _ = []

instance Listable (Positive Int) where
  list = map Positive [1..(maxBound)]


main :: IO ()
main = hspec $ do
  describe "ValidBoardingPass" $ do
    context "Listable ValidBoardingPass" $ do
      it "lists only valid boarding passes: LeanCheck" $
        LC.propertyFor (1024 * 1025) $ propValidBoardingPassIsValid
      modifyMaxSuccess (const (1025 * 10)) $ it "lists only valid boarding passes: QuickCheck" $
        QC.property $ propValidBoardingPassIsValid
  describe "parseBoardingPass :: String -> Maybe BoardingPass" $ do
    context "Derived Testcases" $ do
      it "parseBoardingPass \"FBFBBFFRLR\" ->> Just \"FBFBBFFRLR\"" $
        parseBoardingPass "FBFBBFFRLR" `shouldBe` (Just "FBFBBFFRLR")
      it "parseBoardingPass \"FFFFFFFLLL\" ->> Just \"FFFFFFFLLL\"" $
        parseBoardingPass "FFFFFFFLLL" `shouldBe` Just "FFFFFFFLLL"
      it "parseBoardingPass \"BBBBBBBRRR\" ->> Just \"BBBBBBBRRR\"" $
        parseBoardingPass "BBBBBBBRRR" `shouldBe` Just "BBBBBBBRRR"
      it "parseBoardingPass \"BBBBBBBFRRR\" ->> Nothing" $
        parseBoardingPass "BBBBBBBFRRR" `shouldBe` Nothing
      it "parseBoardingPass \"BBBBBBBRRRL\" ->> Nothing" $
        parseBoardingPass "BBBBBBBRRRL" `shouldBe` Nothing
  describe "stepByStepSeatCoordinatesFromBoardingPass :: BoardingPass -> Maybe [SeatCoordinates]" $ do
    context "Derived Testcases" $ do
      it "stepByStepSeatCoordinatesFromBoardingPass \"FBFBBFFRLR\" ->> Just [((0, 127),(0,7)), ((0,63),(0,7)), ((32,63),(0,7)), ((32,47),(0,7)), ((40,47),(0,7)), ((44,47),(0,7)), ((44,45),(0,7)), ((44,44),(0,7)), ((44,44),(4,7)), ((44,44), (4,5)), ((44,44),(5,5))]" $
       stepByStepSeatCoordinatesFromBoardingPass "FBFBBFFRLR" `shouldBe` (Just [((0, 127),(0,7)), ((0,63),(0,7)), ((32,63),(0,7)), ((32,47),(0,7)), ((40,47),(0,7)), ((44,47),(0,7)), ((44,45),(0,7)), ((44,44),(0,7)), ((44,44),(4,7)), ((44,44), (4,5)), ((44,44),(5,5))])
  describe "(seatIdFromSeatCoordinates 0 8 0 1) :: SeatCoordinates -> Maybe SeatId" $ do
    context "Given Testcases" $ do
      it "seatIdFromSeatCoordinates 0 8 0 1 ((44,44),(5,5)) ->> Just 357" $
        seatIdFromSeatCoordinates 0 8 0 1 ((44,44),(5,5)) `shouldBe` (Just 357)
      it "seatIdFromSeatCoordinates 0 8 0 1 ((70,70),(7,7)) ->> Just 567" $
        seatIdFromSeatCoordinates 0 8 0 1 ((70,70),(7,7)) `shouldBe` (Just 567)
      it "seatIdFromSeatCoordinates 0 8 0 1 ((14,14),(7,7)) ->> Just 119" $
        seatIdFromSeatCoordinates 0 8 0 1 ((14,14),(7,7)) `shouldBe` (Just 119)
      it "seatIdFromSeatCoordinates 0 8 0 1 ((102,102),(4,4)) ->> Just 820" $
        seatIdFromSeatCoordinates 0 8 0 1 ((102,102),(4,4)) `shouldBe` (Just 820)
  describe "(seatIdFromBoardingPassString 0 8 0 1) :: String -> Maybe SeatId" $ do
    context "Given Testcases" $ do
      it "seatIdFromBoardingPassString 0 8 0 1 \"FBFBBFFRLR\" ->> Just 357" $
        seatIdFromBoardingPassString 0 8 0 1 "FBFBBFFRLR" `shouldBe` (Just 357)
      it "seatIdFromBoardingPassString 0 8 0 1 \"BFFFBBFRRR\" ->> Just 567" $
        seatIdFromBoardingPassString 0 8 0 1 "BFFFBBFRRR" `shouldBe` (Just 567)
      it "seatIdFromBoardingPassString 0 8 0 1 \"FFFBBBFRRR\" ->> Just 119" $
        seatIdFromBoardingPassString 0 8 0 1 "FFFBBBFRRR" `shouldBe` (Just 119)
      it "seatIdFromBoardingPassString 0 8 0 1 \"BBFFBBFRLL\" ->> Just 820" $
        seatIdFromBoardingPassString 0 8 0 1 "BBFFBBFRLL" `shouldBe` (Just 820)
    context "Derived Properties" $ do
      it "seatIdFromBoardingPassString is injective: LeanCheck" $
        LC.propertyFor (1024 * 1025) $ propSeatIdFromBoardingPassStringInjective
      modifyMaxSuccess (const (1025 * 128)) $ it "seatIdFromBoardingPassString is injective: QuickCheck" $
        QC.property $ propSeatIdFromBoardingPassStringInjective


propValidBoardingPassIsValid :: ValidBoardingPass -> Bool
propValidBoardingPassIsValid = isJust . parseBoardingPass . getBoardingPass


propSeatIdFromBoardingPassStringInjective :: ValidBoardingPass -> ValidBoardingPass -> Bool
propSeatIdFromBoardingPassStringInjective (ValidBoardingPass p1) (ValidBoardingPass p2) = (seatIdFromBoardingPassString 0 8 0 1 p1 /= seatIdFromBoardingPassString 0 8 0 1 p2) || (p1 == p2)
