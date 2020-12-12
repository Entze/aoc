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
      grid <- readSeats content
      res1 <- task1 grid
      return (res1, 0)

task1 :: MonadError String m => Seats -> m Int
task1 = undefined

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

data Seats = Seats [[Cell]] deriving (Eq, Ord)

instance Show Seats where
  show (Seats []) = ""
  show (Seats [[]]) = ""
  show (Seats ls) = (unlines . map show) ls

readSeats :: MonadError String m => String -> m Seats
readSeats [] = (return . Seats) []
readSeats ls
  | (length . nub . map length) ls' /= 1 = throwError $! "Value Error: Seats malformed."
  | otherwise = (return . Seats) =<< res
  where
    ls' = lines ls
    res = (sequence . map sequence) ((map (map cellFromChar) . lines) ls)

northOf :: MonadError String m => Int -> Int -> Int -> Int -> m (Int,Int)
northOf = undefined

northEastOf :: MonadError String m => Int -> Int -> Int -> Int -> m (Int,Int)
northEastOf = undefined

eastOf :: MonadError String m => Int -> Int -> Int -> Int -> m (Int,Int)
eastOf = undefined

southEastOf :: MonadError String m => Int -> Int -> Int -> Int -> m (Int,Int)
southEastOf = undefined

southOf :: MonadError String m => Int -> Int -> Int -> Int -> m (Int,Int)
southOf = undefined

southWestOf :: MonadError String m => Int -> Int -> Int -> Int -> m (Int,Int)
southWestOf = undefined

westOf :: MonadError String m => Int -> Int -> Int -> Int -> m (Int,Int)
westOf width height x y
  | 0 > x      = throwError $! "Value Error: x coordinate (" ++ (show x) ++ ") below bound (0)."
  | width < x  = throwError $! "Value Error: x coordinate (" ++ (show x) ++ ") below bound (" ++ (show width) ++ ")."
  | 0 > y      = throwError $! "Value Error: y coordinate (" ++ (show y) ++ ") below bound (0)."
  | y < height = throwError $! "Value Error: y coordinate (" ++ (show y) ++ ") below bound (" ++ (show height) ++ ")."
  | 0 > result = throwError $! "Value Error: result (" ++ (show result) ++ ") below bound (0)."
  | otherwise  = return (x-1,y)
    where
      result = x - 1

northWestOf :: MonadError String m => Int -> Int -> Int -> Int -> m (Int,Int)
northWestOf = undefined

nextSeatLayout :: Int -> Int -> Seats -> Seats
nextSeatLayout emptyUntil occupiedWhile seats = undefined
  where
    width = widthOfSeats seats
    height = heightOfSeats seats
    indices = [(x,y) | y <- [0..height], x <- [0..width]]
    getNeighbourFuncs :: (MonadError String m) => [Int -> Int -> m (Int,Int)]
    getNeighbourFuncs = [northOf width height,
                         northEastOf width height,
                         eastOf width height,
                         southEastOf width height,
                         southOf width height,
                         southWestOf width height,
                         westOf width height,
                         northWestOf width height]
    neighboursOfIndex :: (MonadError String m) => [[m (Int,Int)]]
    neighboursOfIndex = [[(uncurry f) i | f <- getNeighbourFuncs] | i <- indices]

widthOfSeats :: Seats -> Int
widthOfSeats (Seats cells) = length (headDef [] cells)

heightOfSeats :: Seats -> Int
heightOfSeats (Seats cells) = length cells


scanFixedPoint :: (MonadError String m,Eq a) => (a -> a) -> a -> m [a]
scanFixedPoint f a
  | null result = throwError $! "Computation Error: Could not find a fixed point."
  | otherwise = return result
  where
    scan = takeUntilEqual . iterate' f
    result = scan a
    takeUntilEqual :: Eq b => [b] -> [b]
    takeUntilEqual (a:b:ls)
      | a == b = [a]
      | otherwise = a:(takeUntilEqual (b:ls))
    takeUntilEqual x = x
