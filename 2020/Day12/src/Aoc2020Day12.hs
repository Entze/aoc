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
  let contentLines = lines content
  ms <- (sequence . map moveFromString) contentLines
  let moveset = reduceMoves ms
  res1 <- task1 moveset
  res2 <- task2 ms
  return (res1, res2)

task1 :: [Move] -> Either String Int
task1 moves = do
  let dest = applyMoves origin moves
  let (Ship x y _) = dest
  return ((abs x) + (abs y))

task2 :: [Move] -> Either String Int
task2 moves = do
  let dest = applyWaypointMoves (Waypoint 0 0) (Waypoint 10 1) moves
  let ((Waypoint x y),_) = dest
  return ((abs x) + (abs y))

-------------------------------------------

data Direction = East | South | West | North deriving (Show, Eq, Ord, Enum, Bounded)

turnBy :: Direction -> Int -> Direction
turnBy dir = (toEnum . (`mod` 4) . (+ (fromEnum dir)))

data Ship = Ship Int Int Direction deriving Eq

data Waypoint = Waypoint Int Int deriving Eq

instance Show Waypoint where
  show (Waypoint x y) = '(':(show x) ++ ',':(show y) ++ ")"

origin :: Ship
origin = Ship 0 0 East

instance Show Ship where
  show (Ship x y dir) = (show x) ++ (',':show y) ++ (' ':(show dir))

data Move = T Direction Int | F Int | R Int | L Int deriving (Eq,Ord)


reduceMoves :: [Move] -> [Move]
reduceMoves = reduceAscMoves . reorderMoves

reduceAscMoves :: [Move] -> [Move]
reduceAscMoves [] = []
reduceAscMoves [h] = reduceMoves' [h]
reduceAscMoves moves = reduceMoves' $! (reduceAscMoves firstHalf) ++ (reduceAscMoves secondHalf)
  where
    (firstHalf, secondHalf) = splitAt half moves'
    half = len `div` 2
    len = length moves'
    moves' = reduceMoves' moves


moveOrdering :: Move -> Move -> Ordering
moveOrdering (F _) (R _) = EQ
moveOrdering (F _) (L _) = EQ
moveOrdering (R _) (F _) = EQ
moveOrdering (L _) (F _) = EQ
moveOrdering x y = compare x y


reorderMoves :: [Move] -> [Move]
reorderMoves = gnomeSortBy moveOrdering

reduceMoves' :: [Move] -> [Move]
reduceMoves' ((R 0):ls)             = reduceMoves' ls
reduceMoves' ((L 0):ls)             = reduceMoves' ls
reduceMoves' ((T _ 0):ls)           = reduceMoves' ls
reduceMoves' ((F 0):ls)             = reduceMoves' ls
reduceMoves' ((L a):(L b):ls)       = reduceMoves' ((L ((a+b) `mod` 360)):ls)
reduceMoves' ((R a):(R b):ls)       = reduceMoves' ((R ((a+b) `mod` 360)):ls)
reduceMoves' ((R a):(L b):ls)       = reduceMoves' ((R ((a-b) `mod` 360)):ls)
reduceMoves' ((L a):(R b):ls)       = reduceMoves' ((R ((-a+b) `mod` 360)):ls)
reduceMoves' ((T d n):ls)
  | n < 0 = reduceMoves' ((T (d `turnBy` 2) (-n)):ls)
reduceMoves' ((x@(T d1 a)):(y@(T d2 b)):ls)
  | d1 ==  d2             = reduceMoves' ((T d1 (a+b)):ls)
  | d1 == (d2 `turnBy` 2) = reduceMoves' ((T d1 (a-b)):ls)
reduceMoves' ((F a):(F b):ls)       = reduceMoves' ((F (a+b)):ls)
reduceMoves' ((L a):ls)             = reduceMoves' ((R ((360 - a) `mod` 360)):ls)
reduceMoves' (x:ls)                 = x:(reduceMoves' ls)
reduceMoves' x                      = x

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
  | d == 'N' = (return . T North . fromJust) n
  | d == 'E' = (return . T East . fromJust) n
  | d == 'S' = (return . T South . fromJust) n
  | d == 'W' = (return . T West . fromJust) n
  | d == 'F' = (return . F . fromJust) n
  | (fromJust n) /= 90 && (fromJust n) /= 180 && (fromJust n) /= 270  = throwError $! "Value Error: Non-valid number of degrees \"" ++ ((show . fromJust) n) ++ "\"."
  | d == 'R' = (return . R . fromJust) n
  | d == 'L' = (return . L . fromJust) n
  | otherwise = throwError $! "Value Error: Unknown action \"" ++ d:"\"."
    where
      n :: Maybe Int
      n = readMay ls

getNumber :: Move -> Int
getNumber (F n) = n
getNumber (R n) = n
getNumber (L n) = n
getNumber (T _ n) = n

applyMove :: Ship -> Move -> Ship
applyMove (Ship x y r) m = Ship x' y' r'
  where
    x' = x + xt
    y' = y + yt
    r' =  r `turnBy` rt
    (xt, yt) = translate m
    rt = rotate m
    rotate (R n) = n `div` 90
    rotate (L n) = -(n `div` 90)
    rotate _ = 0
    translate :: Move -> (Int, Int)
    translate (R _) = (0,0)
    translate (L _) = (0,0)
    translate (F n) = scalePolarity n (polarity r)
    translate (T d n) = scalePolarity n (polarity d)
    polarity :: Direction -> (Int, Int)
    polarity North = (0,1)
    polarity East = (1,0)
    polarity South = (0,-1)
    polarity West = (-1,0)
    scalePolarity :: Int -> (Int,Int) -> (Int, Int)
    scalePolarity n (x,y) = (x * n, y * n)

applyMoves :: Ship -> [Move] -> Ship
applyMoves = foldl' applyMove

oppositeMove :: Move -> Move
oppositeMove (R n) = L n
oppositeMove (L n) = R n
oppositeMove (F n) = F (-n)
oppositeMove (T d n) = T (d `turnBy` 2) n


applyWaypointMove :: Waypoint -> Waypoint -> Move -> (Waypoint, Waypoint)
applyWaypointMove s w (F 0) = (s,w)
applyWaypointMove (Waypoint x y) waypoint@(Waypoint w v) (F n) = (Waypoint x' y', waypoint)
  where
    x' = w * n + x
    y' = v * n + y
applyWaypointMove ship (Waypoint w v) (T North n)     = (ship,           Waypoint    w   (v+n))
applyWaypointMove ship (Waypoint w v) (T South n)     = (ship,           Waypoint    w   (v-n))
applyWaypointMove ship (Waypoint w v) (T East n)      = (ship,           Waypoint   (w+n) v)
applyWaypointMove ship (Waypoint w v) (T West n)      = (ship,           Waypoint   (w-n) v)
applyWaypointMove ship (Waypoint w v) (R 90)          = (ship,           Waypoint    v  (-w))
applyWaypointMove ship (Waypoint w v) (R 180)         = (ship,           Waypoint  (-w) (-v))
applyWaypointMove ship (Waypoint w v) (R 270)         = (ship,           Waypoint  (-v)   w)
applyWaypointMove ship (Waypoint w v) (L 90)          = (ship,           Waypoint  (-v)   w)
applyWaypointMove ship (Waypoint w v) (L 180)         = (ship,           Waypoint  (-w) (-v))
applyWaypointMove ship (Waypoint w v) (L 270)         = (ship,           Waypoint    v  (-w))


applyWaypointMove' :: (Waypoint, Waypoint) -> Move -> (Waypoint, Waypoint)
applyWaypointMove' (s,w) m = applyWaypointMove s w m

applyWaypointMoves :: Waypoint -> Waypoint -> [Move] -> (Waypoint, Waypoint)
applyWaypointMoves s w = foldl' applyWaypointMove' (s,w)
