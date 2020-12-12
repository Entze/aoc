import Aoc2020Day11

import Test.Hspec
import Test.QuickCheck as QC
import Test.Hspec.QuickCheck
import Test.Hspec.LeanCheck as LC

import Data.Either
import Data.List
import Safe


rawInitialSeatLayout :: String
rawInitialSeatLayout = "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"

initialSeatLayout :: Either String Seats
initialSeatLayout = readSeats rawInitialSeatLayout

rawStep1SeatLayout :: String
rawStep1SeatLayout = "#.##.##.##\n#######.##\n#.#.#..#..\n####.##.##\n#.##.##.##\n#.#####.##\n..#.#.....\n##########\n#.######.#\n#.#####.##"

step1SeatLayout :: Either String Seats
step1SeatLayout = readSeats rawStep1SeatLayout

rawStep2SeatLayout :: String
rawStep2SeatLayout = "#.LL.L#.##\n#LLLLLL.L#\nL.L.L..L..\n#LLL.LL.L#\n#.LL.LL.LL\n#.LLLL#.##\n..L.L.....\n#LLLLLLLL#\n#.LLLLLL.L\n#.#LLLL.##"

step2SeatLayout :: Either String Seats
step2SeatLayout = readSeats rawStep2SeatLayout

rawStep3SeatLayout :: String
rawStep3SeatLayout = "#.##.L#.##\n#L###LL.L#\nL.#.#..#..\n#L##.##.L#\n#.##.LL.LL\n#.###L#.##\n..#.#.....\n#L######L#\n#.LL###L.L\n#.#L###.##"

step3SeatLayout :: Either String Seats
step3SeatLayout = readSeats rawStep3SeatLayout

rawStep4SeatLayout :: String
rawStep4SeatLayout = "#.#L.L#.##\n#LLL#LL.L#\nL.L.L..#..\n#LLL.##.L#\n#.LL.LL.LL\n#.LL#L#.##\n..L.L.....\n#L#LLLL#L#\n#.LLLLLL.L\n#.#L#L#.##"

step4SeatLayout :: Either String Seats
step4SeatLayout = readSeats rawStep4SeatLayout

rawStep5SeatLayout :: String
rawStep5SeatLayout = "#.#L.L#.##\n#LLL#LL.L#\nL.#.L..#..\n#L##.##.L#\n#.#L.LL.LL\n#.#L#L#.##\n..L.L.....\n#L#L##L#L#\n#.LLLLLL.L\n#.#L#L#.##"

step5SeatLayout :: Either String Seats
step5SeatLayout = readSeats rawStep5SeatLayout

main :: IO ()
main = hspec $ do
  describe "nextSeatLayout :: Int -> Int -> Seats -> Seats" $ do
    context "Given Testcases" $ do
      it "nextSeatLayout 0 4 intialSeatLayout ->> step1SeatLayout" $
        ((return . nextSeatLayout 0 4) =<< initialSeatLayout) `shouldBe` step1SeatLayout
