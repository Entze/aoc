module Aoc2020Day07 where

import System.Environment (getArgs)
import System.Exit
import Safe

import Data.List
import Data.Maybe
import Text.Regex.TDFA
import qualified Data.Map.Strict as Map

aoc2020Day07Main :: IO ()
aoc2020Day07Main = do
  args <- getArgs
  let file = args `at` 0
  content <- readFile file
  let res = tasks content
  case res of
    Just (first,second) -> (putStrLn . unlines) [show first, show second]
    Nothing -> exitFailure

tasks :: String -> Maybe (Int, Int)
tasks content = do
  first <- task1 content
  second <- task2 content
  return (first, second)


type RawRule = String
type BagDescription = String
type Rule = (BagDescription, RuleExpansions)
type RuleExpansion = (BagDescription, Int)
type RuleExpansions = Map.Map BagDescription Int
type RuleBook = Map.Map BagDescription RuleExpansions

parseRule :: RawRule -> Maybe RawRule
parseRule r
  | r =~ "^[a-z]+ [a-z]+ bags contain [0-9]+ [a-z]+ [a-z]+ bags?(, [0-9]+ [a-z]+ [a-z]+ bags?)*\\.$" || r =~ "^[a-z]+ [a-z]+ bags contain no other bags\\.$" = return r
  | otherwise = Nothing

buildRule :: RawRule -> Maybe Rule
buildRule r = do
  color <- sequence [r' `atMay` 0, r' `atMay` 1]
  let description = unwords color
  let len = length r'
  let inside = if len == 7 then [] else [(r' `at` (i+1) ++ ' ':(r' `at` (i+2)), (read (r' `at` i))) | i <- [4,8..(len-1)]]
  return (description, Map.fromList inside)
  where
    r' :: [RawRule]
    r'= words r

isIrreducable :: RuleBook -> BagDescription -> Bool
isIrreducable rb bd = isJust expansion && (Map.null . fromJust) expansion
  where
    expansion = rb Map.!? bd


expandRule :: RuleBook -> Rule -> Maybe Rule
expandRule rb r@(d,m)
  | Map.null m || (all (isIrreducable rb) . Map.keys) m = return r
  | otherwise = (innerExpansions r) >>= fusedInnerExpansions >>= (return . Map.fromDistinctAscList) >>= (return . ((,) d))
  where
    reduce :: [RuleExpansion] -> [RuleExpansion]
    reduce (one@(d1, c1):(d2, c2):ls)
      | d1 == d2 = reduce ((d1, c1 + c2):ls)
      | otherwise = one:(reduce ((d2,c2):ls))
    reduce x = x
    specialSelect :: RuleBook -> BagDescription -> Maybe RuleExpansions
    specialSelect rb bd
      | isIrreducable rb bd = return (Map.singleton bd 1)
      | otherwise = rb Map.!? bd
    expandInner :: BagDescription -> Int -> Maybe [RuleExpansion]
    expandInner bd n = do
      inner <- specialSelect rb bd
      let innerScaled = Map.map (* n) inner
      (return . reduce . Map.assocs) innerScaled
    innerExpansions :: Rule -> Maybe [[RuleExpansion]]
    innerExpansions (d, m) = (sequence . map (uncurry expandInner) . Map.assocs) m
    fusedInnerExpansions :: [[RuleExpansion]] -> Maybe [RuleExpansion]
    fusedInnerExpansions = return . reduce . sort . concat


reduceRule :: RuleBook -> Rule -> Maybe Rule
reduceRule rb = fixpoint' . (iterate' (expandRule' rb)) . return
  where
    expandRule' :: RuleBook -> Maybe Rule -> Maybe Rule
    expandRule' rb r = r >>= (expandRule rb)
    fixpoint' :: Eq a => [Maybe a] -> Maybe a
    fixpoint' (a:b:ls)
      | a == b = a
      | otherwise = fixpoint' (b:ls)
    fixpoint' _ = Nothing


reduceRuleUntil :: (Rule -> Bool) -> RuleBook -> Rule -> Maybe Rule
reduceRuleUntil predicate rb rule
  | predicate rule || next == return rule = return rule
  | otherwise = (reduceRuleUntil predicate rb) =<< next
  where
    next = expandRule rb rule

reduceRuleUntilBag :: BagDescription -> RuleBook -> Rule -> Maybe Rule
reduceRuleUntilBag bd = reduceRuleUntil (isBag bd)
  where
    isBag :: BagDescription -> Rule -> Bool
    isBag bd (d,m) = bd == d || (isJust . (Map.!? bd)) m

fullyReduceRuleBook :: RuleBook -> Maybe RuleBook
fullyReduceRuleBook rb = do
  rules <- (sequence . map (reduceRule rb) . Map.assocs) rb
  (return . Map.fromList) rules

reduceRuleBook :: BagDescription -> RuleBook -> Maybe RuleBook
reduceRuleBook bd rb = do
   let rules = (Map.assocs rb) :: [Rule]
   reducedRules <- (sequence . map (reduceRuleUntilBag bd rb)) rules
   (return . Map.fromList) reducedRules

countRulesWithBags :: BagDescription -> RuleBook -> Maybe Int
countRulesWithBags bd rb = do
   reducedRulebook <- reduceRuleBook bd rb
   let rules = Map.assocs reducedRulebook
   let filtered = filter (isJust . (Map.!? bd) . snd) rules
   (return . length) filtered

task1 :: String -> Maybe Int
task1 s = do
  parsed <- (sequence . map parseRule) (lines s)
  rules <- (sequence . map buildRule) parsed
  let rulebook = Map.fromList rules
  countRulesWithBags "shiny gold" rulebook

task2 :: String -> Maybe Int
task2 s = do
  parsed <- (sequence . map parseRule) (lines s)
  rules <- (sequence . map buildRule) parsed
  let rulebook = Map.fromList rules
  reducedRulebook <- fullyReduceRuleBook rulebook
  expansion <- reducedRulebook Map.!? "shiny gold"
  (return . sum . Map.elems) expansion
