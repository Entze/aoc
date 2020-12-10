module Aoc2020Day06 where

import System.Environment (getArgs)
import System.Exit
import Safe (at)
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Char

aoc2020Day06Main :: IO ()
aoc2020Day06Main = do
  args <- getArgs
  let file = args `at` 0
  content <- readFile file
  let res = tasks content
  case res of
    Just (first,second) -> (putStrLn . unlines) [show first, show second]
    Nothing -> exitFailure

tasks :: String -> Maybe (Int, Int)
tasks content = do
  first <- countAnsweredQuestionsInQuestionaire content
  second <- countCommonAnsweredQuestionsInQuestionaire content
  return (first, second)

type Person = String
type Group = [Person]

parseData :: String -> Maybe String
parseData d
  | all (\c -> (isAlpha c && isLower c) || isSpace c) d = return d
  | otherwise = Nothing

splitIntoGroups :: String -> Maybe [Group]
splitIntoGroups = return . map lines . splitOn "\n\n"

commonAnsweredQuestions :: Group -> Maybe String
commonAnsweredQuestions = return . foldl' intersect ['a'..'z']

answeredQuestions :: Group -> Maybe String
answeredQuestions = return . unique . sort . filter isAlpha . unlines

countAnsweredQuestionsInQuestionaire :: String -> Maybe Int
countAnsweredQuestionsInQuestionaire d = do
  parsed <- parseData d
  groups <- splitIntoGroups parsed
  answeredQuestionsPerGroup <- (sequence . map answeredQuestions) groups
  let nrOfAnsweredQuestionsPerGroup = map length answeredQuestionsPerGroup
  (return . sum) nrOfAnsweredQuestionsPerGroup

countCommonAnsweredQuestionsInQuestionaire :: String -> Maybe Int
countCommonAnsweredQuestionsInQuestionaire d = do
  parsed <- parseData d
  groups <- splitIntoGroups parsed
  answeredQuestionsPerGroup <- (sequence . map commonAnsweredQuestions) groups
  let nrOfAnsweredQuestionsPerGroup = map length answeredQuestionsPerGroup
  (return . sum) nrOfAnsweredQuestionsPerGroup




unique :: Eq a => [a] -> [a]
unique (a:b:ls)
  | a == b = unique (b:ls)
  | otherwise = a:unique (b:ls)
unique x = x
