module Aoc2020Day17 where

import Safe

import Data.Array
import Data.List
import Data.Maybe
import qualified Data.Text as Text

import AocCommon

aoc2020Day17Main :: IO ()
aoc2020Day17Main = aocMain' instanceFromText instanceToText solve1 solution1ToText solve2 solution2ToText

newtype ProblemInstance = ProblemInstance { dimension :: PocketDimension } deriving (Eq, Ord, Show, Read)
type Solution1 = Int
type Solution2 = Int

instanceFromText :: Text.Text -> ELM ProblemInstance
instanceFromText t
  | Text.null t = throwErrorStringELM "Empty text is not a valid starting dimension."
  | Text.length check /= Text.length t = throwErrorStringELM "Unknown character found."
  | otherwise = (return . ProblemInstance) (listArray ((-d + (1 - (depth `mod` 2)), -w + (1 - (depth `mod` 2)), 0),(d, w, 0)) ls')
  where
    check :: Text.Text
    check = Text.filter (`elem` ['.','#','\n']) t
    ls :: [Text.Text]
    ls = Text.lines t
    ls' :: [Bool]
    ls' = (concat . map (map fromChar) . map Text.unpack) ls
    d = depth `div` 2
    w = width `div` 2
    h = height `div` 2
    depth = length ls
    width = (maximum . map Text.length) ls
    height = 1
    fromChar '#' = True
    fromChar _ = False

solve1 :: ProblemInstance -> ELM Solution1
solve1 p
  | isNothing p'' = throwErrorStringELM "Could not find a solution."
  | otherwise = (return . activeCells . fromJust) p''
  where
    dim = dimension p
    p'' = (headMay . (take 7)) p'
    p' = iterate' progressDimension dim

solve2 :: ProblemInstance -> ELM Solution2
solve2 = undefined

instanceToText :: ProblemInstance -> Text.Text
instanceToText p
    | minZ == maxZ && minZ == 0 = (Text.pack . show' . assocs) dim
    | otherwise = (Text.pack . show) dim
    where
      show' [] = []
      show' [(_, a)] = [show'' a]
      show' (((x1, _, _),a1):(b@((x2, _, _), a2)):ls)
        | x1 /= x2 = (show'' a1):'\n':(show'' a2):(show' ls)
        | otherwise = (show'' a1):(show' (b:ls))
      show'' True = '#'
      show'' False = '.'
      dim = dimension p
      ((_,_,minZ),(_,_,maxZ)) = bounds dim



solution1ToText :: Solution1 -> Text.Text
solution1ToText = (Text.pack . show)

solution2ToText :: Solution2 -> Text.Text
solution2ToText = (Text.pack . show)

type Coordinates = (Int, Int, Int)
type PocketDimension = Array Coordinates Bool

origin :: Coordinates
origin = (0,0,0)

neighbourCoordinates :: Coordinates -> [Coordinates]
neighbourCoordinates (x, y, z) = [(x',y',z') | x' <- [(x-1)..(x+1)], y' <- [(y-1)..(y+1)], z' <- [(z-1)..(z+1)], x /= x' || y /= y' || z /= z']

nullDimension :: PocketDimension
nullDimension = listArray ((0,0,0),(0,0,0)) [False]

nullDimensionOf :: Int -> Int -> Int -> PocketDimension
nullDimensionOf depth width height = array ((-d' + (1-(depth `mod` 2)),
                                             -w' + (1-(width `mod` 2)),
                                             -h' + (1-(height `mod` 2))),
                                             (d', w', h'))
                                     [((x, y, z), False) |
                                      x <- [(-d' + (1-(depth `mod` 2)))..d'],
                                      y <- [(-w' + (1-(width `mod` 2)))..w'],
                                      z <- [(-h' + (1-(height `mod` 2)))..h']]
  where
    w' = width `div` 2
    h' = height `div` 2
    d' = depth `div` 2

(!?) :: Ix i => Array i e -> i -> Maybe e
(!?) arr i
  | (bounds arr) `inRange` i = return (arr ! i)
  | otherwise = Nothing

numberOfActiveNeighbours :: Coordinates -> PocketDimension -> Int
numberOfActiveNeighbours c dims = (length . filter id) neighbours
  where
    ncs = neighbourCoordinates c
    maybeNeighbours = map (dims !?) ncs
    neighbours = map (maybe False id) maybeNeighbours

progressDimension :: PocketDimension -> PocketDimension
progressDimension dim = (array (bounds dim') . (map progress) . assocs) dim'
  where
    dim' = enlargeDimensionBy dim 1
    progress :: (Coordinates, Bool) -> (Coordinates, Bool)
    progress (c,a) = (c, ans == 3 || (ans == 2 && a))
      where
        ans :: Int
        ans = numberOfActiveNeighbours c dim'

enlargeDimensionBy dim n = array ((xMin - n, yMin - n, zMin - n), (xMax + n, yMax + n, zMax + n)) ((assocs dim) ++ new)
  where
    ((xMin, yMin, zMin),(xMax, yMax, zMax)) = bounds dim
    new :: [(Coordinates, Bool)]
    new = [((x,y,z), False) |
           x <- [(xMin - n)..(xMax + n)],
           y <- [(yMin - n)..(yMax + n)],
           z <- [(zMin - n)..(zMax + n)], (x < xMin || y < yMin || z < zMin) || (x > xMax || y > yMax || z > zMax)]



activeCells :: PocketDimension -> Int
activeCells = length . filter id .  elems
