--{-# LANGUAGE FlexibleContexts #-}
module Aoc2020Day12 where

import System.Environment (getArgs)
import System.Exit
import Safe
import Control.Monad.Except

import Data.Maybe
import Data.Either
import Data.List

import AocCommon

aoc2020Day12Main :: IO ()
aoc2020Day12Main = do
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
tasks content = do
      return (0, 0)

task1 :: String -> Either String Int
task1 = undefined

-------------------------------------------

data Direction = North | East | South | West deriving (Show, Eq, Ord, Enum, Bounded)

data Ship = Ship Int Int Direction deriving Eq

instance Show Ship where
  show (Ship x y dir) = (show x) ++ (',':show y) ++ (' ':(show dir))

data Move = F Int | R Int | L Int | T Direction Int deriving Eq

instance Show Move where
  show (F n) = 'F':(show n)
  show (R n) = 'R':(show n)
  show (L n) = 'L':(show n)
  show (T North n) = 'N':(show n)
  show (T East n) = 'E':(show n)
  show (T South n) = 'S':(show n)
  show (T West n) = 'W':(show n)

moveFromString :: String -> Either String Move
moveFromString (d:ls)
  | isNothing n = throwError $! "Value Error: Could not parse \"" ++ ls ++ "\" as Int."
  | d == 'F' = (return . F . fromJust) n
  | d == 'R' = (return . R . fromJust) n
  | d == 'L' = (return . R . fromJust) n
  | d == 'N' = (return . T North . fromJust) n
  | d == 'E' = (return . T East . fromJust) n
  | d == 'S' = (return . T South . fromJust) n
  | d == 'W' = (return . T West . fromJust) n
  | otherwise = throwError $! "Value Error: Unknown action \"" ++ d:"\"."
    where
      n :: Maybe Int
      n = readMay ls
