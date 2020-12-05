module Aoc2020Day05 where

import Safe (lastMay)

type BoardingPass = String
type SeatCoordinates = ((Int, Int), (Int, Int))
type SeatId = Int

parseBoardingPass :: String -> Maybe BoardingPass
parseBoardingPass n
  | (length . take 11) n == 10 && (all (`elem` "FB") . take 7) n && (all (`elem` "LR") . drop 7) n = return n
  | otherwise = Nothing

stepByStepSeatCoordinatesFromBoardingPass :: BoardingPass -> Maybe [SeatCoordinates]
stepByStepSeatCoordinatesFromBoardingPass = (return . scanl specialise ((0,127),(0,7)))
  where
    specialise ((rowMin, rowMax), col) 'F' = ((rowMin, (rowMin + rowMax) `div` 2), col)
    specialise ((rowMin, rowMax), col) 'B' = (((rowMin + rowMax + 1) `div` 2, rowMax), col)
    specialise (row, (colMin, colMax)) 'L' = (row, (colMin, (colMin + colMax) `div` 2))
    specialise (row, (colMin, colMax)) 'R' = (row, ((colMin + colMax + 1) `div` 2, colMax))
    specialise coords _ = coords

seatIdFromSeatCoordinates :: Int -> Int -> Int -> Int -> SeatCoordinates -> Maybe SeatId
seatIdFromSeatCoordinates rowOffset rowScale columnOffset columnScale ((r1,r2),(s1,s2))
  | r1 == r2 && s1 == s2 = return ((r1 * rowScale) + rowOffset + (s1 * columnScale) + columnOffset)
  | otherwise = Nothing


seatIdFromBoardingPassString :: Int -> Int -> Int -> Int -> String -> Maybe SeatId
seatIdFromBoardingPassString rowOffset rowScale columnOffset columnScale p = do
  p' <- parseBoardingPass p
  coords <- stepByStepSeatCoordinatesFromBoardingPass p'
  coord <- lastMay coords
  id <- seatIdFromSeatCoordinates rowOffset rowScale columnOffset columnScale coord
  return id
