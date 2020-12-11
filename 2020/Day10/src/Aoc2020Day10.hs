module Aoc2020Day10 where

import System.Environment (getArgs)
import System.Exit
import Safe

import Data.Maybe
import Data.Either
import Data.List

aoc2020Day10Main :: IO ()
aoc2020Day10Main = do
  args <- getArgs
  let file = args `at` 0
  content <- readFile file
  let result = tasks content
  case result of
    Left err -> do
      putStrLn err
      exitFailure
    Right (v1, v2) ->
      do
        (putStrLn . show) v1
        (putStrLn . show) v2
        exitSuccess

tasks :: String -> Either String (Int,Int)
tasks content
  | null contentLines = Left "No lines read."
  | otherwise = do
      order1 <- contentInts >>= (return . sort)
      diffs <- differences order1
      let diff1 = (length . filter (==1)) diffs
      let diff2 = 1 + (length . filter (==3)) diffs
      let task1 = diff1 * diff2
      return (task1, 0)
  where
    contentLines :: [String]
    contentLines = lines content
    contentInts :: Either String [Int]
    contentInts = sequence $ map readSafe contentLines
    readSafe :: (Read a) => String -> Either String a
    readSafe s
      | isNothing r = Left ("Could not read \"" ++ s ++ "\".")
      | otherwise = (Right . fromJust) r
      where
        r = readMay s



-------------------------------------------


inOrder :: [Int] -> Bool
inOrder [] = True
inOrder ls@(h:_) = 0 < h && h <= 3 && inOrder' ls
  where
    inOrder' (j1:j2:ls) = j2 - 3 <= j1 && j1 <= j2 - 1 && inOrder' ls
    inOrder' _ = True

order :: Int -> Int -> [Int] -> Either String [Int]
order _ _ [] = return []
order mi ma w
  | null oi || (not . any isJust) solsMaybe = Left "No feasible solution found."
  | otherwise = (return . fromJust . fromJust . headMay . filter isJust) solsMaybe
  where
    oi :: [[Int]]
    oi = orderIndices mi ma w
    sols :: [[Maybe Int]]
    sols = map (map (atMay w)) oi
    solsMaybe :: [Maybe [Int]]
    solsMaybe = map sequence sols



orderIndices :: Int -> Int -> [Int] -> [[Int]]
orderIndices _ _ [] = []
orderIndices mi ma w = orderIndices' (length w) [[]]
  where
    orderIndices' 0 is = is
    orderIndices' n is = orderIndices' (n-1) [i ++ [c] | i <- is, c <- nextCandidateIndices mi ma i w]

nextCandidateIndices :: Int -> Int -> [Int] -> [Int] -> [Int]
nextCandidateIndices mi ma w ls
  | null w = filterCorrectElems 0
  | otherwise = filterCorrectElems (ls `at` (last w))
  where
    filterCorrectElems n = (map fst . filter ((inRangeOf (n+mi) (n+ma)) . snd) . filter ((`notElem` w) . fst)) (zip [0..] ls)


inRangeOf mi ma e = mi <= e && e <= ma

differences :: [Int] -> Either String [Int]
differences [h] = Right [h]
differences ls@(h:_) = sequence $ (Right h):(differences' ls)
  where
    differences' :: [Int] -> [Either String Int]
    differences' [h,l] = [Right (l - h)]
    differences' (h:n:ls) = (Right (n - h)) : (differences' (n:ls))
    differences' [h] = [Right h]
    differences' [] = []
differences [] = Left "No list to apply differences."
