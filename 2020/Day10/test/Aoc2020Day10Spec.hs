import Aoc2020Day10

import Test.Hspec
import Test.QuickCheck as QC
import Test.Hspec.QuickCheck
import Test.Hspec.LeanCheck as LC

import Data.Either
import Data.List
import Safe


list1 :: [Int]
list1 = [16,10,15,5,1,11,7,19,6,12,4]
list2 :: [Int]
list2 = [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]

main :: IO ()
main = hspec $ do
  describe "order :: [Int] -> Either String [Int]" $ do
    context "Derived Properties" $ do
      it "should terminate: LeanCheck" $
        LC.propertyFor 10 $ (propTerminate (order 1 3) :: ([Int] -> Bool))
      it "should only produce lists in order: short LeanCheck" $
        LC.propertyFor 10000 $ propOrderEitherErrorOrInOrder
      it "should be idempotent: LeanCheck" $
        LC.propertyFor 10000 $ propOrderIsIdempotent
      it "should be closed: LeanCheck" $
        LC.propertyFor 10000 $ propOrderIsClosed
      modifyMaxSuccess (const 100) $ it "should only produce lists in order: QuickCheck" $
        QC.property $ propOrderEitherErrorOrInOrder
      modifyMaxSuccess (const 100) $ it "should be idempotent: QuickCheck" $
        QC.property $ propOrderIsIdempotent
      modifyMaxSuccess (const 100) $ it "should be closed: QuickCheck" $
        QC.property $ propOrderIsClosed
    context "Given Testcases" $ do
      it "length . filter (== 1) . differences . order list1 ->> 7" $
        ((return . length . filter (== 1)) =<< differences =<< order 1 3 list1) `shouldBe` Right 7
      it "length . filter (== 3) . differences . order list1 ->> 5" $
        ((return . length . filter (== 3)) =<< differences =<< order 1 3 list1) `shouldBe` Right 4
      it "length . filter (== 1) . differences . order list2 ->> 22" $
        ((return . length . filter (== 1)) =<< differences =<< order 1 3 list2) `shouldBe` Right 22
      it "length . filter (== 3) . differences . order list2 ->> 10" $
        ((return . length . filter (== 3)) =<< differences =<< order 1 3 list2) `shouldBe` Right 9
      it "order list1 ->> [1,4,5,6,7,10,11,12,15,16,19]" $
        (order 1 3 list1) `shouldBe` Right [1,4,5,6,7,10,11,12,15,16,19]
  describe "nextCandidateIndices :: Int -> Int -> [Int] -> [Int] -> [Int]" $ do
    context "Given Testcases" $ do
      it "nextCandidateIndices 1 3 [] list1 ->> [4]" $
        nextCandidateIndices 1 3 [] list1 `shouldBe` [4]
      it "nextCandidateIndices 1 3 [4] list1 ->> [10]" $
        nextCandidateIndices 1 3 [4] list1 `shouldBe` [10]
      it "nextCandidateIndices 1 3 [4,10] list1 ->> [3,6,8]" $
        (sort . nextCandidateIndices 1 3 [4,10]) list1 `shouldBe` [3,6,8]
      it "nextCandidateIndices 1 3 [4,10,3] list1 ->> [6,8]" $
        (sort . nextCandidateIndices 1 3 [4,10,3]) list1 `shouldBe` [6,8]
      it "nextCandidateIndices 1 3 [4,10,3,8] list1 ->> [6]" $
        (sort . nextCandidateIndices 1 3 [4,10,3,8]) list1 `shouldBe` [6]
      it "nextCandidateIndices 1 3 [4,10,3,8,6] list1 ->> [1]" $
        (sort . nextCandidateIndices 1 3 [4,10,3,8,6]) list1 `shouldBe` [1]
      it "nextCandidateIndices 1 3 [4,10,3,8,6,1] list1 ->> [5,9]" $
        (sort . nextCandidateIndices 1 3 [4,10,3,8,6,1]) list1 `shouldBe` [5,9]
      it "nextCandidateIndices 1 3 [4,10,3,8,6,1,5] list1 ->> [9]" $
        (sort . nextCandidateIndices 1 3 [4,10,3,8,6,1,5]) list1 `shouldBe` [9]
      it "nextCandidateIndices 1 3 [4,10,3,8,6,1,5,9] list1 ->> [2]" $
        (sort . nextCandidateIndices 1 3 [4,10,3,8,6,1,5,9]) list1 `shouldBe` [2]
      it "nextCandidateIndices 1 3 [4,10,3,8,6,1,5,9,2] list1 ->> [0]" $
        (sort . nextCandidateIndices 1 3 [4,10,3,8,6,1,5,9,2]) list1 `shouldBe` [0]
      it "nextCandidateIndices 1 3 [4,10,3,8,6,1,5,9,2,0] list1 ->> [7]" $
        (sort . nextCandidateIndices 1 3 [4,10,3,8,6,1,5,9,2,0]) list1 `shouldBe` [7]
      it "nextCandidateIndices 1 3 [4,10,3,8,6,1,5,9,2,0,7] list1 ->> []" $
        (sort . nextCandidateIndices 1 3 [4,10,3,8,6,1,5,9,2,0,7]) list1 `shouldBe` []
  describe "differences :: [Int] -> Either String [Int]" $ do
    context "Derived Properties" $ do
      it "should terminate" $
        LC.propertyFor 10 $ (propTerminate differences)
      it "should be reconstructible: LeanCheck" $
        LC.propertyFor 10000 $ propReconstructibleList
      modifyMaxSuccess (const 1000) $ it "should be reconstructible: LeanCheck" $
        QC.property $ propReconstructibleList




propTerminate f a = f a == f a

propOrderEitherErrorOrInOrder :: [Int] -> Bool
propOrderEitherErrorOrInOrder ls = isLeft ls' || (inOrder . (fromRight [10])) ls'
  where
    ls' = order 1 3 ls

propOrderIsIdempotent :: [Int] -> Bool
propOrderIsIdempotent ls = (first >>= order 1 3) == first
  where
    first = order 1 3 ls

propOrderIsClosed :: [Int] -> Bool
propOrderIsClosed ls = isLeft first || (first == (((return . sort) :: ([Int] -> Either String [Int])) ls))
  where
    first = ((order 1 3) ls >>= (return . sort))


propReconstructibleList :: [Int] -> Bool
propReconstructibleList [] = True
propReconstructibleList ls = isRight diff && (tail (scanl' (+) 0 (fromRight [] diff))) == ls
  where
    diff = differences ls
