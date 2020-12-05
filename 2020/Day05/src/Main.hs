module Main where

import Aoc2020Day05

import System.Environment (getArgs)
import System.Exit
import Safe (at, minimumMay, maximumMay, headMay)
import Data.Maybe

main = do
  args <- getArgs
  content <- readFile (args `at` 0)
  let contentLines = ((lines content) :: [String])
  let res = do
        let seatIds = (((map fromJust . filter isJust . map (seatIdFromBoardingPassString 0 8 0 1)) contentLines) :: [SeatId])
        max <- maximumMay seatIds
        min <- minimumMay seatIds
        let candidateSeats = [s | s <- [min.. max], (not.(elem s)) seatIds, elem (s-1) seatIds, elem (s+1) seatIds]
        mySeat <- headMay candidateSeats
        return (max,mySeat)
  case res of
    Just (max,mySeat) -> (putStrLn . unlines . map show) [max,mySeat]
    Nothing -> exitFailure
