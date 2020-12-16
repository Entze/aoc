module Aoc2020Day15 where

import System.Environment (getArgs)
import System.Exit
import Safe
import Control.Monad.Except

import Data.Maybe
import Data.Either
import Data.List

import qualified Data.Text as Text
import qualified Data.IntMap.Strict as IntMap

import AocCommon

aoc2020Day15Main :: IO ()
aoc2020Day15Main = aocMain' instanceFromText instanceToText solve1 solution1ToText solve2 solution2ToText

newtype StartingList = StartingList [Int] deriving (Show, Eq, Ord, Read)
newtype Solution1 = Sol1 Int deriving (Show, Eq, Ord, Read)
newtype Solution2 = Sol2 Int deriving (Show, Eq, Ord, Read)

getList :: StartingList -> [Int]
getList (StartingList list) = list


instanceFromText :: Text.Text -> ELM StartingList
instanceFromText text
  | Text.null text = throwErrorStringELM "Read empty file."
  | null ls = throwErrorStringELM "Lines empty."
  | otherwise = (return . StartingList . map (read . Text.unpack) . Text.split (== ',') . head) ls
  where
    ls = Text.lines text

solve1 :: StartingList -> ELM Solution1
solve1 = return . Sol1 . (`shoutInfOptAt` (2020 - 1)) . getList

solve2 :: StartingList -> ELM Solution2
solve2 = return . Sol2 . (`shoutInfOptAt` (30000000 - 1)) . getList

instanceToText :: StartingList -> Text.Text
instanceToText = Text.pack . (joinStringsBy ',') . (map show) . getList

solution1ToText :: Solution1 -> Text.Text
solution1ToText (Sol1 i) = (Text.pack . show) i

solution2ToText :: Solution2 -> Text.Text
solution2ToText (Sol2 i) = (Text.pack . show) i

shoutInf :: [Int] -> [Int]
shoutInf [] = []
shoutInf start = start ++ shoutInf' start

shoutInf' :: [Int] -> [Int]
shoutInf' ls = n:ns
  where
    n = shoutNext ls
    ns = shoutInf' (ls ++ [n])



baseMap :: [Int] -> IntMap.IntMap Int
baseMap = IntMap.fromList . (`zip` [0..]) . init

shoutInfOptAt :: [Int] -> Int -> Int
shoutInfOptAt [] _  = 0
shoutInfOptAt start i
  | i < startingIndex = start `at` i
  | otherwise = (head . IntMap.keys) (shoutInfOptAt' i startingIndex (last start) base)
  where
    base = baseMap start
    startingIndex = length start

shoutInfOptAt' :: Int -> Int -> Int -> IntMap.IntMap Int -> IntMap.IntMap Int
shoutInfOptAt' goal i l m = shoutInfOpt'' i l m
  where
    shoutInfOpt'' i l m
      | i' > goal = IntMap.filter (== goal) m
      | otherwise = shoutInfOptAt' goal (i+1) n m'
      where
        i' = i - 1
        n = shoutNextOpt i l m
        m' = IntMap.insert l i' m

shoutNextOpt :: Int -> Int -> IntMap.IntMap Int -> Int
shoutNextOpt i l m = i' - IntMap.findWithDefault i' l m
  where
    i' = i - 1


shoutNext :: [Int] -> Int
shoutNext [] = 0
shoutNext ls = through 1 s sl
  where
    (s:sl) = reverse ls
    through _ _ [] = 0
    through c n (x:xs)
      | n == x = c
      | otherwise = through (c+1) n xs
