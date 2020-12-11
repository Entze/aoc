{-# LANGUAGE FlexibleContexts #-}
module Aoc2020Day11 where

import System.Environment (getArgs)
import System.Exit
import Safe
import Control.Monad.Except

import Data.Maybe
import Data.Either
import Data.List

aoc2020Day11Main :: IO ()
aoc2020Day11Main = do
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

tasks :: MonadError String m => String -> m (Int,Int)
tasks content = do
      grid <- readGrid content
      return (0, 0)


-------------------------------------------

data Cell = Floor | Empty | Occupied deriving (Eq, Ord, Enum)

instance Show Cell where
  show Floor = "."
  show Empty = "L"
  show Occupied = "#"

cellFromString :: MonadError String m => String -> m Cell
cellFromString "." = return Floor
cellFromString "L" = return Empty
cellFromString "#" = return Occupied
cellFromString s = throwError $! "Value Error: Could not parse \"" ++ s ++ "\" to Cell."

cellFromChar :: MonadError String m => Char -> m Cell
cellFromChar '.' = return Floor
cellFromChar 'L' = return Empty
cellFromChar '#' = return Occupied
cellFromChar c = throwError $! "Value Error: Could not parse \"" ++ (c:"\" to Cell.")

data Seats = Grid [[Cell]] deriving (Eq, Ord)

instance Show Seats where
  show (Grid []) = ""
  show (Grid [[]]) = ""
  show (Grid ls) = (unlines . map show) ls

readGrid :: MonadError String m => String -> m Seats
readGrid [] = (return . Grid) []
readGrid ls
  | (length . nub . map length) ls /= 1 = throwError $! "Value Error: Grid malformed."
  | otherwise = (return . Grid) =<< res
  where
    res = (sequence . map sequence) ((map (map cellFromChar) . lines) ls)


westOf :: MonadError String m => Int -> Int -> Int -> Int -> m Int
westOf width height x y
  | 0 > x      = throwError $! "Value Error: x coordinate (" ++ (show x) ++ ") below bound (0)."
  | width < x  = throwError $! "Value Error: x coordinate (" ++ (show x) ++ ") below bound (" ++ (show width) ++ ")."
  | 0 > y      = throwError $! "Value Error: y coordinate (" ++ (show y) ++ ") below bound (0)."
  | y < height = throwError $! "Value Error: y coordinate (" ++ (show y) ++ ") below bound (" ++ (show height) ++ ")."
  | 0 > result = throwError $! "Value Error: result (" ++ (show result) ++ ") below bound (0)."
  | otherwise  = return (x-1)
    where
      result = x - 1
