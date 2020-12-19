import Test.Hspec
import Test.QuickCheck as QC hiding ((===))
import Test.Hspec.QuickCheck
import Test.Hspec.LeanCheck as LC
import Test.Hspec.SmallCheck as SC
import Test.LeanCheck.Utils.Operators

import Safe
import Aoc2020Day18
import AocCommon.AocMain

import Data.Either
import Data.Either.Combinators
import qualified Data.Text as Text


expr1Text = Text.pack "2 * 3 + (4 * 5)"
expr1 = [VAL (Text.singleton '2'), OP (Text.singleton '*'), VAL (Text.singleton '3'), OP (Text.singleton '+') , LPAREN, VAL (Text.singleton '4'), OP (Text.singleton '*'), VAL (Text.singleton '5'), RPAREN]
expr1Order = [
  VAL (Text.singleton '2'),
  VAL (Text.singleton '3'),
  OP (Text.singleton '*'),
  VAL (Text.singleton '4'),
  VAL (Text.singleton '5'),
  OP (Text.singleton '*'),
  OP (Text.singleton '+')
  ]

expr2Text = Text.pack "5 + (8 * 3 + 9 + 3 * 4 * 3)"
expr3Text = Text.pack "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
expr4Text = Text.pack "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

testProblemInstances = instanceFromText (Text.unlines [expr1Text,expr2Text,expr3Text,expr4Text])


instance Arbitrary Token where
  arbitrary = arbitrary >>= (return . toEnum)

instance Listable Token where
  list = map toEnum [0..(maxBound :: Int)]

newtype ValidExpr = ValidExpr { getExpr :: [Token] } deriving (Show, Read, Eq, Ord)

instance Arbitrary ValidExpr where
  arbitrary = sized sizedArbitrary
    where
      sizedArbitrary :: Int -> Gen ValidExpr
      sizedArbitrary 0 = (return . ValidExpr . (:[]) . VAL . Text.pack . show) =<< chooseInt (0, 16)
      sizedArbitrary n = do
        allParen <- arbitrary
        leftParen <- arbitrary
        rightParen <- arbitrary
        operator <- elements [OP (Text.singleton '+'), OP (Text.singleton '*')]
        split <- chooseInt (0,(n-1))
        split' <- (return . ((n-1) -)) split
        left' <- sizedArbitrary split
        right' <- sizedArbitrary split'
        left <- (return . getExpr) left'
        right <- (return . getExpr) right'
        (return . ValidExpr) (makeExpr allParen leftParen rightParen left operator right)
      makeExpr :: Bool -> Bool -> Bool -> [Token] -> Token -> [Token] -> [Token]
      makeExpr allParen leftParen rightParen leftExpr operator rightExpr = e'
        where
          l' = if leftParen then LPAREN:leftExpr ++ [RPAREN] else leftExpr
          r' = if rightParen then LPAREN:rightExpr ++ [RPAREN] else rightExpr
          e = l' ++ (operator:r')
          e' = if allParen then LPAREN:e ++ [RPAREN] else e
  shrink (ValidExpr [VAL n]) = [ValidExpr [VAL ((Text.pack . show) n')] | n' <- ((shrink . (read :: String -> Int ) . Text.unpack) n)]
  shrink (ValidExpr (LPAREN:ls))
    | last ls == RPAREN = [ValidExpr (init ls)]
    | otherwise = (map (ValidExpr . (LPAREN:) . getExpr)) (shrink (ValidExpr ls))
  shrink (ValidExpr ((VAL n1):(OP o):(VAL n2):ls))
    | o == Text.singleton '+' = [ ValidExpr $ pre:(getExpr post) | pre <- [(VAL .Text.pack . show) n | n <- shrink np ], post <- shrink (ValidExpr ls)]
    | otherwise               = [ ValidExpr $ pre:(getExpr post) | pre <- [(VAL .Text.pack . show) n | n <- shrink nt ], post <- shrink (ValidExpr ls)]
      where
        n1' = ((read :: String -> Int) . Text.unpack) n1
        n2' = ((read :: String -> Int) . Text.unpack) n2
        np = n1' + n2'
        nt = n1' * n2'
  shrink _ = []

instance Listable ValidExpr where
  list = foldr (+|) [] (map list' [0..])

list' :: Int -> [ValidExpr]
list' 0 = map ValidExpr [[(VAL . Text.pack . show) n] | n <- ([0..16] :: [Int])]
list' n = map ValidExpr [ llP ++ (getExpr l) ++ lrP ++ ((o:rlP) ++ (getExpr r)) ++ rrP |
                          l' <- [0..(n - 1)],
                          r' <- [(n-1) - l'],
                          l <- list' l' ,
                          o <- [(OP . Text.singleton) '+', (OP . Text.singleton) '*'],
                          r <- list' r',
                          (llP,lrP) <- [([],[]),([LPAREN], [RPAREN])],
                          (rlP,rrP) <- [([],[]),([LPAREN], [RPAREN])]]

main :: IO ()
main = hspec $ do
  describe "Token" $ do
    context "Enum Token" $ do
      it "toEnum . fromEnum == id: LeanCheck" $
        LC.propertyFor 10000 $ ((toEnum :: Int -> Token) . fromEnum) === (id)
      modifyMaxSuccess (const 1000) $ it "toEnum . fromEnum == id: QuickCheck" $
        QC.property $ ((toEnum :: Int -> Token) . fromEnum) === (id)
  describe "ValidExpr" $ do
    context "Listable ValidExpr" $ do
      it "produces only wellformed expressions: LeanCheck" $
        LC.propertyFor 10000 $ wellFormed . getExpr
    context "Arbitrary ValidExpr" $ do
      modifyMaxSuccess (const 1000) $ it "produces only Wellformed expressions: QuickCheck" $
        QC.property $ wellFormed . getExpr
  describe "wellFormed :: [Token] -> Bool" $ do
    context "Derived Testcases" $ do
      it "wellFormed expr1 ->> True" $
        wellFormed expr1 `shouldBe` True
  describe "areParensBalanced :: [Token] -> Bool" $ do
    context "Derived Testcases" $ do
      it "areParensBalanced [] ->> True" $
        areParensBalanced [] `shouldBe` True
      it "areParensBalanced [LPAREN,RPAREN] ->> True" $
        areParensBalanced [LPAREN,RPAREN] `shouldBe` True
      it "areParensBalanced expr1 ->> True" $
        areParensBalanced expr1 `shouldBe` True
      it "areParensBalanced (LPAREN:expr1 ++ [RPAREN]) ->> True" $
        areParensBalanced (LPAREN:expr1 ++ [RPAREN]) `shouldBe` True
      it "areParensBalanced (LPAREN:expr1) ->> False" $
        areParensBalanced (LPAREN:expr1) `shouldBe` False
      it "areParensBalanced (expr1 ++ [RPAREN]) ->> False" $
        areParensBalanced (expr1 ++ [RPAREN]) `shouldBe` False
      it "areParensBalanced (RPAREN:expr1) ->> False" $
        areParensBalanced (RPAREN:expr1) `shouldBe` False
  describe "instanceFromText :: Text.text -> ELM ProblemInstance" $ do
    context "Given Testcases" $ do
      it "instanceFromText expr1Text ->> TokenizedText [expr1]" $
        instanceFromText expr1Text `shouldBe` (return . TokenizedText) [expr1]
  describe "orderPostfixLefftAssoc (getPrecedence equalPrecedence) (const False)" $ do
    context "Given Testcases" $ do
      it "orderPostfixLeftAssoc (getPrecedence equalPrecedence) (const False) expr1 ->> expr1Order" $
        (fst . fromRight' . runELM . orderPostfixLeftAssoc (getPrecedence equalPrecedence) (const False)) expr1 `shouldBe` (fst . fromRight' . runELM . return) expr1Order

  describe "evaluate :: [Token]" $ do
    context "Given Testcases" $ do
      it "evaluate expr1Order ->> 26" $
        evaluate expr1Order `shouldBe` 26
  describe "solve1 :: ProblemInstance -> Solution1" $ do
    context "Given Testcases" $ do
      it "solve1 testProblemInstances ->> 26 + 437 + 12240 + 13632" $
        (solve1 =<< testProblemInstances) `shouldBe` return (26 + 437 + 12240 + 13632)
