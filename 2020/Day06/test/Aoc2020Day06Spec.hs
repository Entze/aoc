{-# #-}

import Aoc2020Day06

import Test.Hspec
import Test.QuickCheck as QC
import Test.Hspec.QuickCheck
import Test.Hspec.LeanCheck as LC

import Data.Maybe
import Data.List

main :: IO ()
main = hspec $ do
  describe "unique :: Eq a => [a] -> [a]" $ do
    context "Derived Properties" $ do
      it "is idempotent: LeanCheck" $
        LC.propertyFor 100000 $ (propUniqueIdempotent :: [Int] -> Bool)
      it "unique . sort === sort . nub: LeanCheck" $
        LC.propertyFor 100000 $ (propSortUniqueEquivalentNub :: [Int] -> Bool)
      modifyMaxSuccess (const 1000) $ it "is idempotent: QuickCheck" $
        QC.property $ (propUniqueIdempotent :: [Int] -> Bool)
      modifyMaxSuccess (const 1000) $ it "unique . sort === sort . nub: QuickCheck" $
        QC.property $ (propSortUniqueEquivalentNub :: [Int] -> Bool)
  describe "splitIntoGroups :: String -> Maybe Group" $ do
    context "Given Testcases" $ do
      it "splitIntoGroups \"abcx\nabcy\nabcz\n\" ->> Just [[\"abcx\",\"abcy\",\"abcz\"]]" $
        splitIntoGroups "abcx\nabcy\nabcz\n" `shouldBe` (Just [["abcx","abcy","abcz"]])
      it "splitIntoGroups \"abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\" ->> Just [[\"abc\"], [\"a\",\"b\",\"c\"], [\"ab\",\"ac\"], [\"a\",\"a\",\"a\",\"a\"], [\"b\"]]" $
        splitIntoGroups "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb" `shouldBe` (Just [["abc"], ["a","b","c"], ["ab","ac"], ["a","a","a","a"], ["b"]])
  describe "answeredQuestions :: Group -> Maybe String" $ do
    context "Given Testcases" $ do
      it "answeredQuestions [\"abcx\",\"abcy\",\"abcz\"] ->> Just \"abcxyz\"" $
        answeredQuestions ["abcx","abcy","abcz"] `shouldBe` (Just "abcxyz")
      it "answeredQuestions [\"abc\"] ->> Just \"abc\"" $
          answeredQuestions ["abc"] `shouldBe` (Just "abc")
      it "answeredQuestions [\"a\",\"b\",\"c\"] ->> Just \"abc\"" $
          answeredQuestions ["a","b","c"] `shouldBe` (Just "abc")
      it "answeredQuestions [\"ab\",\"ac\"] ->> Just \"abc\"" $
          answeredQuestions ["ab","ac"] `shouldBe` (Just "abc")
      it "answeredQuestions [\"a\",\"a\",\"a\",\"a\"] ->> Just \"a\"" $
          answeredQuestions ["a","a","a","a"] `shouldBe` (Just "a")
      it "answeredQuestions [\"b\"] ->> Just \"b\"" $
          answeredQuestions ["b"] `shouldBe` (Just "b")
  describe "countAnsweredQuestionsInQuestionaire :: String -> Maybe Int" $ do
    context "Given Testcases" $ do
      it "countAnsweredQuestionsInQuestionaire \"abcx\nabcy\nabcz\n\" ->> Just 6" $
        countAnsweredQuestionsInQuestionaire "abcx\nabcy\nabcz\n" `shouldBe` (Just 6)
      it "countAnsweredQuestionsInQuestionaire \"abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\" ->> Just 11" $
        countAnsweredQuestionsInQuestionaire "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb" `shouldBe` (Just 11)
  describe "countCommonAnsweredQuestionsInQuestionaire :: String -> Maybe Int" $ do
    context "Given Testcases" $ do
      it "countCommonAnsweredQuestionsInQuestionaire \"abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\" ->> Just 6" $
        countCommonAnsweredQuestionsInQuestionaire "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb" `shouldBe` (Just 6)


propUniqueIdempotent :: Eq a => [a] -> Bool
propUniqueIdempotent ls = ls' == unique ls'
  where
    ls' = unique ls

propSortUniqueEquivalentNub :: Ord a => [a] -> Bool
propSortUniqueEquivalentNub ls = ((unique . sort) ls) == ((sort . nub) ls)
