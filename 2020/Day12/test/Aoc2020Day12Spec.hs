import Aoc2020Day12

import Test.Hspec
import Test.QuickCheck as QC
import Test.Hspec.QuickCheck
import Test.Hspec.LeanCheck as LC

import Data.Either
import Data.List
import Safe


moveSet = [F 10, T North 3, F 7, R 90, F 11]


instance Listable Direction where
  list = [minBound..maxBound]

instance Listable Move where
  list = [ f i | i <-[0..(maxBound :: Int)], f <- (( F:(zipWith ($) (repeat T) (list :: [Direction]))))] ++ [ f i | i <- [90, 180, 270], f <- [R,L]]


-- (0,0) (0,1) (1,0) (1,1) (0,2)
instance Listable Ship where
  list = iterate' next' origin
    where
      next' (Ship x y North)
        | x == y     = Ship  0    (y+1) East
        | (x+1) == y = Ship (x+1)  0    East
        | x > y      = Ship  x    (y+1) East
        | x < y      = Ship (x+1)  y    East
      next' (Ship x y d) = Ship x y (d `turnBy` 1)

instance Arbitrary Direction where
  arbitrary = elements list
  shrink East = []
  shrink South = [East]
  shrink West = [South, East]
  shrink North = [West, South, East]

instance Arbitrary Move where
  arbitrary = do
    i <- arbitrary :: Gen (Positive Int)
    m <- elements [F, T East, T South, T West, T North, R . (* 90) . (+1) . (`mod` 3), L . (* 90) . (+1) . (`mod` 3)]
    (return . m . getPositive) i
  shrink (T _ n) = [T d' n' | d' <- [East, South, West, North], n' <- shrink n]
  shrink (R n) = [f n' | f <- [R,L], n' <- [90, 180, 270], n > n']
  shrink (L n) = [f n' | f <- [L,R], n' <- [90, 180, 270], n > n']
  shrink (F n) = [F n' | n' <- shrink n]


instance Arbitrary Ship where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    d <- arbitrary
    return (Ship x y d)
  shrink (Ship x y _) = [ Ship x' y' d' | d' <- [East, South, West, North], x' <- shrink x, y' <- shrink y]

main :: IO ()
main = hspec $ do
  describe "applyMove :: Ship -> Move -> Ship" $ do
    context "Given Testcases" $ do
      it "applyMove origin (moveSet !! 0) ->> Ship 10 0 East" $
        applyMove origin (moveSet `at` 0) `shouldBe` Ship 10 0 East
      it "applyMove (Ship 10 0 East) (moveSet !! 1) ->> Ship 10 3 East" $
        applyMove (Ship 10 0 East) (moveSet `at` 1) `shouldBe` Ship 10 3 East
      it "applyMove (Ship 10 3 East) (moveSet !! 2) ->> Ship 17 3 East" $
        applyMove (Ship 10 3 East) (moveSet `at` 2) `shouldBe` Ship 17 3 East
      it "applyMove (Ship 17 3 East) (moveSet !! 3) ->> Ship 17 3 South" $
        applyMove (Ship 17 3 East) (moveSet `at` 3) `shouldBe` Ship 17 3 South
      it "applyMove (Ship 17 3 South) (moveSet !! 4) ->> Ship 17 -8 South" $
        applyMove (Ship 17 3 South) (moveSet `at` 4) `shouldBe` Ship 17 (-8) South
      it "applyMove (Ship 17 -8 South) (T West 5) ->> Ship 12 -8 South" $
        applyMove (Ship 17 (-8) South) (T West 5) `shouldBe` Ship 12 (-8) South
      it "applyMove (Ship 17 -8 South) (L 270) ->> Ship 17 -8 West" $
        applyMove (Ship 17 (-8) South) (L 270) `shouldBe` Ship 17 (-8) West
  describe "oppositeMove :: Move -> Move" $ do
    context "Derived Testcases" $ do
      it "applyMove (applyMove (Ship 0 0 West) (F 1)) (oppositeMove (F 1)) ->> Ship 0 0 West" $
        applyMove (applyMove (Ship 0 0 West) (F 1)) (oppositeMove (F 1)) `shouldBe` Ship 0 0 West
    context "Given Properties" $ do
      it "(opposite . opposite) == id: LeanCheck" $
        LC.propertyFor 1000000 $ propOppositeTwiceShouldEqual
      it "should take back to origin: LeanCheck" $
        LC.propertyFor 1000000 $ propOppositeMoveTakesBack
      modifyMaxSuccess (const 1000) $ it "(opposite . opposite) == id: QuickCheck" $
        QC.property $ propOppositeTwiceShouldEqual
      modifyMaxSuccess (const 1000) $ it "should take back to origin: QuickCheck" $
        QC.property $ propOppositeMoveTakesBack
  describe "turnBy :: Direction -> Int -> Direction" $ do
    context "Derived Properties" $ do
      it "should not turn with 0: LeanCheck" $
        LC.propertyFor 9 $ propDirectionTurnBy0IsId
      modifyMaxSuccess (const 100) $ it "should not turn with 0: QuickCheck" $
        QC.property $ propDirectionTurnBy0IsId
    context "Derived Testcases" $ do
      it "turnBy North 1 ->> East" $
        turnBy North 1 `shouldBe` East
      it "turnBy West 1 ->> North" $
        turnBy West 1 `shouldBe` North
      it "turnBy North 3 ->> West" $
        turnBy North 3 `shouldBe` West
      it "turnBy West 3 ->> South" $
        turnBy West 3 `shouldBe` South
      it "turnBy North -1 ->> West" $
        turnBy North (-1) `shouldBe` West
      it "turnBy West -1 ->> South" $
        turnBy West (-1) `shouldBe` South
      it "turnBy North -3 ->> East" $
        turnBy North (-3) `shouldBe` East
      it "turnBy West -3 ->> North" $
        turnBy West (-3) `shouldBe` North
  describe "applyMoves :: Ship -> [Move] -> Ship" $ do
    context "Given Testcases" $ do
      it "applyMoves origin moveSet ->> Ship 17 -8 South" $
        applyMoves origin moveSet `shouldBe` Ship 17 (-8) South
    context "Derived Properties" $ do
      it "should be consistent: LeanCheck" $
        LC.propertyFor 1000000 $ propApplyMoveIsConsistent
      modifyMaxSuccess (const 1000) $ it "should be consistent: QuickCheck" $
        QC.property $ propApplyMoveIsConsistent
  describe "reorderMoves :: [Move] -> [Move]" $ do
    context "Derived Testcases" $ do
      it "reorderMoves [F 2, L 90, T East 0, F 1] ->> [T East 0, F 2, L 90, F 1]" $
        reorderMoves[F 2, L 90, T East 0, F 1] `shouldBe` [T East 0, F 2, L 90, F 1]
    context "Derived Properties" $ do
      it "should terminate : LeanCheck" $
        LC.propertyFor 100 $ propTerminates1 reorderMoves
      it "should be idempotent: LeanCheck" $
        LC.propertyFor 10000 $ propIsIdempotent1 reorderMoves
      modifyMaxSuccess (const 1000) $ it "should be idempotent: QuickCheck" $
        QC.property $ propIsIdempotent1 reorderMoves
  describe "reduceMoves :: [Move] -> [Move]" $ do
    context "Derived Testcases " $ do
      it "reduceMoves [T South 10, R 90, T North 10] ->> [R 90]" $
        reduceMoves [T South 10, R 90, T North 10] `shouldBe` [R 90]
      it "reduceMoves [T South 10, R 90, F 10] ->> [T South 10, R 90, F 10]" $
        reduceMoves [T South 10, R 90, F 10] `shouldBe` [T South 10, R 90, F 10]
      it "reduceMoves [F 10, R 90, F 10] ->> [F 10, R 90, F 10]" $
        reduceMoves [F 10, R 90, F 10] `shouldBe` [F 10, R 90, F 10]
      it "reduceMoves [T South 1, T South 1, R 90] ->> [T South 2, R 90]" $
        reduceMoves [T South 1, T South 1, R 90] `shouldBe` [T South 2, R 90]
      it "reduceMoves [R 90, T South 1, T South 1] ->> [T South 2, R 90]" $
        reduceMoves [R 90, T South 1, T South 1] `shouldBe` [T South 2, R 90]
      it "reduceMoves [F 1, R 90, L 90, F 1] ->> [F 2]" $
        reduceMoves [F 1, R 90, L 90, F 1] `shouldBe` [F 2]
      it "reduceMoves [F 2, L 90, T East 0, F 1] ->> [F 2, R 270, F 1]" $
        reduceMoves [F 2, L 90, T East 0, F 1] `shouldBe` [F 2, R 270, F 1]
      it "reduceMoves [F 1,R 90,L 180,R 90,F 1] ->> [F 2]" $
        reduceMoves [F 1,R 90 ,L 180,R 90 ,F 1] `shouldBe` [F 2]
    context "Derived Properties" $ do
      it "should terminate : LeanCheck" $
        LC.propertyFor 100 $ propTerminates1 reduceMoves
      it "should be idempotent: LeanCheck" $
        LC.propertyFor 10000 $ propIsIdempotent1 reduceMoves
      it "should be consistent: LeanCheck" $
        LC.propertyFor 10000 $ propReduceMovesIsConsistent
      modifyMaxSuccess (const 1000) $ it "should be idempotent: QuickCheck" $
        QC.property $ propIsIdempotent1 reduceMoves
      modifyMaxSuccess (const 1000) $ it "should be consistent: QuickCheck" $
        QC.property $ propReduceMovesIsConsistent
  describe "applyWaypointMoves :: Waypoint -> Waypoint -> [Move] -> (Waypoint, Waypoint)" $ do
    context "Given Testcases" $ do
      it "applyWaypointMoves Waypoint 0 0 Waypoint 10 1 moveSet ->> (Waypoint 214 -72, _)" $
        (fst . applyWaypointMoves (Waypoint 0 0) (Waypoint 10 1)) moveSet `shouldBe` Waypoint 214  (-72)
  describe "applyWaypointMove :: Ship -> Move -> Ship" $ do
    context "Given Testcases" $ do
      it "applyWaypointMove (Waypoint 0 0) (Waypoint 10 1) (moveSet !! 0) ->> (Waypoint 100 10, Waypoint 10 1)" $
        applyWaypointMove (Waypoint 0 0) (Waypoint 10 1) (moveSet `at` 0) `shouldBe` (Waypoint 100 10, Waypoint 10 1)
      it "applyWaypointMove (Waypoint 100 10) (Waypoint 10 1) (moveSet !! 1) ->> (Waypoint 100 10, Waypoint 10 4)" $
        applyWaypointMove (Waypoint 100 10) (Waypoint 10 1) (moveSet `at` 1) `shouldBe` (Waypoint 100 10, Waypoint 10 4)
      it "applyWaypointMove (Waypoint 100 10) (Waypoint 10 4) (moveSet !! 2) ->> (Waypoint 170 38, Waypoint 10 4)" $
        applyWaypointMove (Waypoint 100 10) (Waypoint 10 4) (moveSet `at` 2) `shouldBe` (Waypoint 170 38, Waypoint 10 4)
      it "applyWaypointMove (Waypoint 170 38) (Waypoint 10 4) (moveSet !! 3) ->> (Waypoint 170 38, Waypoint 4 -10)" $
        applyWaypointMove (Waypoint 170 38) (Waypoint 10 4) (moveSet `at` 3) `shouldBe` (Waypoint 170 38, Waypoint 4 (-10))
      it "applyWaypointMove (Waypoint 170 38) (Waypoint 4 -10) (moveSet !! 4) ->> (Waypoint 214 -72, Waypoint 4 10)" $
        applyWaypointMove (Waypoint 170 38) (Waypoint 4 (-10)) (moveSet `at` 4) `shouldBe` (Waypoint 214 (-72), Waypoint 4 (-10))
      it "applyWaypointMove (Waypoint 0 0) (Waypoint 1 2) (R 90) ->> (Waypoint 0 0, Waypoint 2 -1)" $
        applyWaypointMove (Waypoint 0 0) (Waypoint 1 2) (R 90) `shouldBe` (Waypoint 0 0, Waypoint 2 (-1))
      it "applyWaypointMove (Waypoint 0 0) (Waypoint 1 2) (R 180) ->> (Waypoint 0 0, Waypoint -1 -2)" $
        applyWaypointMove (Waypoint 0 0) (Waypoint 1 2) (R 180) `shouldBe` (Waypoint 0 0, Waypoint (-1) (-2))
      it "applyWaypointMove (Waypoint 0 0) (Waypoint 1 2) (R 270) ->> (Waypoint 0 0, Waypoint -2 1)" $
        applyWaypointMove (Waypoint 0 0) (Waypoint 1 2) (R 270) `shouldBe` (Waypoint 0 0, Waypoint (-2) 1)



propTerminates1 :: (a -> b) -> a -> Bool
propTerminates1 f a = (const True) $! (f a)

propIsIdempotent1 :: Eq a => (a -> a) -> a -> Bool
propIsIdempotent1 f a = (f a') == a'
  where
    a' = f a

propOppositeTwiceShouldEqual :: Move -> Bool
propOppositeTwiceShouldEqual m = m == (oppositeMove . oppositeMove) m

propOppositeMoveTakesBack :: Ship -> Move -> Bool
propOppositeMoveTakesBack ship move = ship == applyMove dest opposite
  where
    dest :: Ship
    dest = applyMove ship move
    opposite :: Move
    opposite = oppositeMove move

propDirectionTurnBy0IsId :: Direction -> Bool
propDirectionTurnBy0IsId dir = dir == dir `turnBy` 0


propReduceMovesIsConsistent :: Ship -> [Move] -> Bool
propReduceMovesIsConsistent ship moves = ship `applyMoves` moves == ship `applyMoves` (reduceMoves moves)

propApplyMoveIsConsistent :: Ship -> Move -> Bool
propApplyMoveIsConsistent ship@(Ship x y East)  move@(F n)       = applyMove ship move == (Ship (x+n)  y    East)
propApplyMoveIsConsistent ship@(Ship x y South) move@(F n)       = applyMove ship move == (Ship  x    (y-n) South)
propApplyMoveIsConsistent ship@(Ship x y West)  move@(F n)       = applyMove ship move == (Ship (x-n)  y    West)
propApplyMoveIsConsistent ship@(Ship x y North) move@(F n)       = applyMove ship move == (Ship  x    (y+n) North)
propApplyMoveIsConsistent ship@(Ship x y East)  move@(R 90)      = applyMove ship move == (Ship  x     y    South)
propApplyMoveIsConsistent ship@(Ship x y South) move@(R 90)      = applyMove ship move == (Ship  x     y    West)
propApplyMoveIsConsistent ship@(Ship x y West)  move@(R 90)      = applyMove ship move == (Ship  x     y    North)
propApplyMoveIsConsistent ship@(Ship x y North) move@(R 90)      = applyMove ship move == (Ship  x     y    East)
propApplyMoveIsConsistent ship@(Ship x y East)  move@(R 180)     = applyMove ship move == (Ship  x     y    West)
propApplyMoveIsConsistent ship@(Ship x y South) move@(R 180)     = applyMove ship move == (Ship  x     y    North)
propApplyMoveIsConsistent ship@(Ship x y West)  move@(R 180)     = applyMove ship move == (Ship  x     y    East)
propApplyMoveIsConsistent ship@(Ship x y North) move@(R 180)     = applyMove ship move == (Ship  x     y    South)
propApplyMoveIsConsistent ship@(Ship x y East)  move@(R 270)     = applyMove ship move == (Ship  x     y    North)
propApplyMoveIsConsistent ship@(Ship x y South) move@(R 270)     = applyMove ship move == (Ship  x     y    East)
propApplyMoveIsConsistent ship@(Ship x y West)  move@(R 270)     = applyMove ship move == (Ship  x     y    South)
propApplyMoveIsConsistent ship@(Ship x y North) move@(R 270)     = applyMove ship move == (Ship  x     y    West)
propApplyMoveIsConsistent ship@(Ship x y East)  move@(L 90)      = applyMove ship move == (Ship  x     y    North)
propApplyMoveIsConsistent ship@(Ship x y South) move@(L 90)      = applyMove ship move == (Ship  x     y    East)
propApplyMoveIsConsistent ship@(Ship x y West)  move@(L 90)      = applyMove ship move == (Ship  x     y    South)
propApplyMoveIsConsistent ship@(Ship x y North) move@(L 90)      = applyMove ship move == (Ship  x     y    West)
propApplyMoveIsConsistent ship@(Ship x y East)  move@(L 180)     = applyMove ship move == (Ship  x     y    West)
propApplyMoveIsConsistent ship@(Ship x y South) move@(L 180)     = applyMove ship move == (Ship  x     y    North)
propApplyMoveIsConsistent ship@(Ship x y West)  move@(L 180)     = applyMove ship move == (Ship  x     y    East)
propApplyMoveIsConsistent ship@(Ship x y North) move@(L 180)     = applyMove ship move == (Ship  x     y    South)
propApplyMoveIsConsistent ship@(Ship x y East)  move@(L 270)     = applyMove ship move == (Ship  x     y    South)
propApplyMoveIsConsistent ship@(Ship x y South) move@(L 270)     = applyMove ship move == (Ship  x     y    West)
propApplyMoveIsConsistent ship@(Ship x y West)  move@(L 270)     = applyMove ship move == (Ship  x     y    North)
propApplyMoveIsConsistent ship@(Ship x y North) move@(L 270)     = applyMove ship move == (Ship  x     y    East)
propApplyMoveIsConsistent ship@(Ship x y d)     move@(T East n)  = applyMove ship move == (Ship (x+n)  y    d)
propApplyMoveIsConsistent ship@(Ship x y d)     move@(T South n) = applyMove ship move == (Ship  x    (y-n) d)
propApplyMoveIsConsistent ship@(Ship x y d)     move@(T West n)  = applyMove ship move == (Ship (x-n)  y    d)
propApplyMoveIsConsistent ship@(Ship x y d)     move@(T North n) = applyMove ship move == (Ship  x    (y+n) d)
propApplyMoveIsConsistent _ _ = False
