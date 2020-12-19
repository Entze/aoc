module Aoc2020Day18 where

import Safe

import Control.Monad.Except

import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text

import AocCommon

aoc2020Day18Main :: IO ()
aoc2020Day18Main = aocMain' instanceFromText instanceToText solve1 solution1ToText solve2 solution2ToText

data Token = LPAREN | RPAREN | OP {op :: Text.Text} | VAL {val :: Text.Text} deriving (Show, Read, Eq, Ord)

isParen LPAREN = True
isParen RPAREN = True
isParen _ = False

isOp (OP _) = True
isOp _ = False

isPlus t = isOp t && (op t) == (Text.singleton '+')

isMult t = isOp t && (op t) == (Text.singleton '*')

isVal (VAL _) = True
isVal _ = False

tokenToText :: Token -> Text.Text
tokenToText LPAREN = Text.singleton '('
tokenToText RPAREN = Text.singleton ')'
tokenToText (OP o) = o
tokenToText (VAL n) = n

instance Enum Token where
  toEnum i
    | i == 0 = LPAREN
    | i == 1 = RPAREN
    | i == 2 = OP (Text.singleton '+')
    | i == 3 = OP (Text.singleton '*')
    | i < 0 = (VAL . Text.pack . show) i
    | otherwise = (VAL . Text.pack . show . (flip (-) 4)) i
  fromEnum LPAREN = 0
  fromEnum RPAREN = 1
  fromEnum (OP o) = 2 + ((fromEnum . maybe False (== '*') . headMay . Text.unpack) o)
  fromEnum (VAL n)
    | n' >= 0 = n' + 4
    | otherwise = n'
    where
      n' = ((read . Text.unpack) n)



type ProblemInstance = TokenizedText
type Solution1 = Int
type Solution2 = Int

newtype TokenizedText = TokenizedText { tokenRows :: [[Token]] } deriving (Show, Read, Eq, Ord)

instanceFromText :: Text.Text -> ELM ProblemInstance
instanceFromText t
  | all wellFormed tokenized = (return . TokenizedText) tokenized
  | otherwise = throwErrorStringELM "Encountered a malformed string."
  where
    splitByLines :: [Text.Text]
    splitByLines = Text.lines t
    splitByWords :: [[Text.Text]]
    splitByWords = map Text.words splitByLines
    tokenized :: [[Token]]
    tokenized = map concat tokenized'
    tokenized' :: [[[Token]]]
    tokenized' = map (map fromText) splitByWords
    fromText :: Text.Text -> [Token]
    fromText t
      | Text.null t = []
      | Text.head t == '(' = LPAREN:(fromText (Text.tail t))
      | Text.last t == ')' = (fromText (Text.init t)) ++ [RPAREN]
      | Text.all isDigit t = [VAL t]
      | otherwise = [OP t]

wellFormed :: [Token] -> Bool
wellFormed ts = all ($ ts) [areParensBalanced, noSuccessiveOps, noSuccessiveVals]

areParensBalanced :: [Token] -> Bool
areParensBalanced t = all (>= 0) parenEval && (maybe False (== 0) . lastMay) parenEval
  where
    toValue LPAREN = 1
    toValue RPAREN = -1
    toValue _ = 0
    parenEval :: [Int]
    parenEval = ((scanl' (+) 0) . map toValue) t

noSuccessiveOps :: [Token] -> Bool
noSuccessiveOps [] = True
noSuccessiveOps ((OP _):(OP _):_) = False
noSuccessiveOps (a:b:ls) = noSuccessiveOps ls
noSuccessiveOps _ = True

noSuccessiveVals :: [Token] -> Bool
noSuccessiveVals [] = True
noSuccessiveVals ((VAL _):(VAL _):_) = False
noSuccessiveVals (a:b:ls) = noSuccessiveOps ls
noSuccessiveVals _ = True


solve1 :: ProblemInstance -> ELM Solution1
solve1 = undefined

solve2 :: ProblemInstance -> ELM Solution2
solve2 = undefined

instanceToText :: ProblemInstance -> Text.Text
instanceToText = (Text.unlines . map Text.concat . map (map toText) . tokenRows)
  where
    toText :: Token -> Text.Text
    toText LPAREN = Text.singleton '('
    toText RPAREN = Text.singleton ')'
    toText (OP o) = ' ' `Text.cons` o `Text.snoc` ' '
    toText (VAL n) = n

solution1ToText :: Solution1 -> Text.Text
solution1ToText = (Text.pack . show)

solution2ToText :: Solution2 -> Text.Text
solution2ToText = (Text.pack . show)

evaluate :: [Token] -> Int
evaluate = undefined

equalPrecedence :: Map.Map Token Int
equalPrecedence = Map.empty

getPrecedence :: Map.Map Token Int -> Token -> Int
getPrecedence m = maybe 5 id . (m Map.!?)

orderPostfixLeftAssoc :: (Token -> Int) -> (Token -> Bool) -> [Token] -> ELM [Token]
orderPostfixLeftAssoc precedence isRightAssociative = orderPostfixLeftAssoc' [] []
  where
    orderPostfixLeftAssoc' :: [Token] -> [Token] -> [Token] -> ELM [Token]
    orderPostfixLeftAssoc' out stack [] = do
      logStringELM $! ("Finished parsing " ++ (show ((length stack) + (length out)) ) ++ " elements with " ++ (show (length stack)) ++ " operators left on the stack.")
      (return . reverse) ((reverse stack) ++ out)
    orderPostfixLeftAssoc' out [] (tokens@(t:ts))
      | isVal t = do
          logELM ((Text.pack "VAL ") <> (val t) <> (Text.pack $! (" tokens -> out-queue. out: " ++ ((show . length) out) ++ " op-stack: 0")))
          orderPostfixLeftAssoc' (t:out) [] ts
      | isOp t = do
          logELM ((Text.pack "OP  ") <> (op t) <> (Text.pack $! (" tokens -> op-stack. out: " ++ ((show . length) out) ++ " op-stack: 0")))
          orderPostfixLeftAssoc' out [t] ts
      | t == LPAREN = do
          logELM ((Text.pack "(   ") <> (Text.pack $! (" tokens -> op-stack. out: " ++ ((show . length) out) ++ " op-stack: 0")))
          orderPostfixLeftAssoc' out [t] ts
    orderPostfixLeftAssoc' out (stack@(s:ss)) (tokens@(t:ts))
      | isVal t = do
          logELM ((Text.pack "VAL ") <> (val t) <> (Text.pack $! (" tokens -> out-queue. out: " ++ ((show . length) out) ++ " op-stack: " ++ ((show . length) stack))))
          orderPostfixLeftAssoc' (t:out) stack ts
      | isOp t && s /= LPAREN && ((pt < ps) || (pt == ps && (not . isRightAssociative) t)) = do
          logELM ((Text.pack "OP  ") <> (op s) <> (Text.pack $! (" op-stack -> out-queue. out: " ++ ((show . length) out) ++ " op-stack: " ++ ((show . length) stack))))
          orderPostfixLeftAssoc' (s:out) ss tokens
      | isOp t = do
          logELM ((Text.pack "OP  ") <> (op t) <> (Text.pack $! (" op-stack -> out-queue. out: " ++ ((show . length) out) ++ " op-stack: " ++ ((show . length) stack))))
          orderPostfixLeftAssoc' out (t:stack) ts
      | t == LPAREN = do
          logELM ((Text.pack "(    ") <> (Text.pack $! (" tokens -> op-stack. out: " ++ ((show . length) out) ++ " op-stack: " ++ ((show . length) stack))))
          orderPostfixLeftAssoc' out (t:stack) ts
      | t == RPAREN && s == LPAREN = do
          logELM ((Text.pack ")    ") <> (Text.pack $! (" tokens -> delete. out: " ++ ((show . length) out) ++ " op-stack: " ++ ((show . length) stack))))
          logELM ((Text.pack "(    ") <> (Text.pack $! (" tokens -> delete. out: " ++ ((show . length) out) ++ " op-stack: " ++ ((show . length) stack))))
          orderPostfixLeftAssoc' out ss ts
      | t == RPAREN = do
          logELM ((Text.pack "OP  ") <> (op s) <> (Text.pack $! (" op-stack -> out-queue. out: " ++ ((show . length) out) ++ " op-stack: " ++ ((show . length) stack))))
          orderPostfixLeftAssoc' (s:out) ss tokens
      where
        pt = precedence t
        ps = precedence s
    orderPostfixLeftAssoc' out stack tokens = throwError $! ((map Text.pack ["Unknown pattern:", "out:"]) ++ [Text.intercalate (Text.singleton ',') (map tokenToText out)] ++ (map Text.pack ["stack:"]) ++ [Text.intercalate (Text.singleton ',') (map tokenToText stack)] ++ (map Text.pack ["tokens:"]) ++ [Text.intercalate (Text.singleton ',') (map tokenToText tokens)])
