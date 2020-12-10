{-# #-}

import Aoc2020Day07

import Test.Hspec
import Test.QuickCheck as QC
import Test.Hspec.QuickCheck
import Test.Hspec.LeanCheck as LC

import Data.Maybe
import Data.List
import Safe
import qualified Data.Map.Strict as Map


main :: IO ()
main = hspec $ do
  describe "buildRule :: RawRule -> Maybe (BagDescription, Rule)" $ do
    context "Given Testcases" $ do
      it "buildRule exampleRawRules !! 0 ->> Just exampleRules !! 0" $
        (buildRule =<< (exampleRawRules `atMay` 0)) `shouldBe` (exampleRules `atMay` 0)
      it "buildRule exampleRawRules !! 1 ->> Just exampleRules !! 1" $
        (buildRule =<< (exampleRawRules `atMay` 1)) `shouldBe` (exampleRules `atMay` 1)
      it "buildRule exampleRawRules !! 2 ->> Just exampleRules !! 2" $
        (buildRule =<< (exampleRawRules `atMay` 2)) `shouldBe` (exampleRules `atMay` 2)
      it "buildRule exampleRawRules !! 3 ->> Just exampleRules !! 3" $
        (buildRule =<< (exampleRawRules `atMay` 3)) `shouldBe` (exampleRules `atMay` 3)
      it "buildRule exampleRawRules !! 4 ->> Just exampleRules !! 4" $
        (buildRule =<< (exampleRawRules `atMay` 4)) `shouldBe` (exampleRules `atMay` 4)
      it "buildRule exampleRawRules !! 5 ->> Just exampleRules !! 5" $
        (buildRule =<< (exampleRawRules `atMay` 5)) `shouldBe` (exampleRules `atMay` 5)
      it "buildRule exampleRawRules !! 6 ->> Just exampleRules !! 6" $
        (buildRule =<< (exampleRawRules `atMay` 6)) `shouldBe` (exampleRules `atMay` 6)
      it "buildRule exampleRawRules !! 7 ->> Just exampleRules !! 7" $
        (buildRule =<< (exampleRawRules `atMay` 7)) `shouldBe` (exampleRules `atMay` 7)
      it "buildRule exampleRawRules !! 8 ->> Just exampleRules !! 8" $
        (buildRule =<< (exampleRawRules `atMay` 8)) `shouldBe` (exampleRules `atMay` 8)
  describe "reduceRule :: RuleBook -> Rule -> Maybe Rule" $ do
    context "Given Testcases" $ do
      it "reduceRule exampleRulebook (exampleRules !! 0) ->> exampleFullyExpandedRules !! 0" $
        ((reduceRule exampleRulebook) =<< (exampleRules `atMay` 0)) `shouldBe` (exampleFullyExpandedRules `atMay` 0)
      it "reduceRule exampleRulebook (exampleRules !! 1) ->> exampleFullyExpandedRules !! 1" $
        ((reduceRule exampleRulebook) =<< (exampleRules `atMay` 1)) `shouldBe` (exampleFullyExpandedRules `atMay` 1)
      it "reduceRule exampleRulebook (exampleRules !! 2) ->> exampleFullyExpandedRules !! 2" $
        ((reduceRule exampleRulebook) =<< (exampleRules `atMay` 2)) `shouldBe` (exampleFullyExpandedRules `atMay` 2)
      it "reduceRule exampleRulebook (exampleRules !! 3) ->> exampleFullyExpandedRules !! 3" $
        ((reduceRule exampleRulebook) =<< (exampleRules `atMay` 3)) `shouldBe` (exampleFullyExpandedRules `atMay` 3)
      it "reduceRule exampleRulebook (exampleRules !! 4) ->> exampleFullyExpandedRules !! 4" $
        ((reduceRule exampleRulebook) =<< (exampleRules `atMay` 4)) `shouldBe` (exampleFullyExpandedRules `atMay` 4)
      it "reduceRule exampleRulebook (exampleRules !! 5) ->> exampleFullyExpandedRules !! 5" $
        ((reduceRule exampleRulebook) =<< (exampleRules `atMay` 5)) `shouldBe` (exampleFullyExpandedRules `atMay` 5)
      it "reduceRule exampleRulebook (exampleRules !! 6) ->> exampleFullyExpandedRules !! 6" $
        ((reduceRule exampleRulebook) =<< (exampleRules `atMay` 6)) `shouldBe` (exampleFullyExpandedRules `atMay` 6)
      it "reduceRule exampleRulebook (exampleRules !! 7) ->> exampleFullyExpandedRules !! 7" $
        ((reduceRule exampleRulebook) =<< (exampleRules `atMay` 7)) `shouldBe` (exampleFullyExpandedRules `atMay` 7)
      it "reduceRule exampleRulebook (exampleRules !! 8) ->> exampleFullyExpandedRules !! 8" $
        ((reduceRule exampleRulebook) =<< (exampleRules `atMay` 8)) `shouldBe` (exampleFullyExpandedRules `atMay` 8)
  describe "reduceRuleUnitl :: (Rule -> Bol) -> RuleBook -> Rule -> Rule" $ do
    context "Given Testcases" $ do
      it "reduceRuleUntil (isJust . (Map.!? \"shiny gold\") . snd) exampleRuleBook (exampleRules !! 0) ->> exampleExpandedRules !! 0" $
        ((reduceRuleUntil (isJust . (Map.!? ("shiny gold" :: BagDescription)) . snd) exampleRulebook) =<< (exampleRules `atMay` 0)) `shouldBe` (exampleExpandedRules `atMay` 0)
      it "reduceRuleUntil (isJust . (Map.!? \"shiny gold\") . snd) exampleRuleBook (exampleRules !! 1) ->> exampleExpandedRules !! 1" $
        ((reduceRuleUntil (isJust . (Map.!? ("shiny gold" :: BagDescription)) . snd) exampleRulebook) =<< (exampleRules `atMay` 1)) `shouldBe` (exampleExpandedRules `atMay` 1)
      it "reduceRuleUntil (isJust . (Map.!? \"shiny gold\") . snd) exampleRuleBook (exampleRules !! 2) ->> exampleExpandedRules !! 2" $
        ((reduceRuleUntil (isJust . (Map.!? ("shiny gold" :: BagDescription)) . snd) exampleRulebook) =<< (exampleRules `atMay` 2)) `shouldBe` (exampleExpandedRules `atMay` 2)
      it "reduceRuleUntil (isJust . (Map.!? \"shiny gold\") . snd) exampleRuleBook (exampleRules !! 3) ->> exampleExpandedRules !! 3" $
        ((reduceRuleUntil (isJust . (Map.!? ("shiny gold" :: BagDescription)) . snd) exampleRulebook) =<< (exampleRules `atMay` 3)) `shouldBe` (exampleExpandedRules `atMay` 3)
      it "reduceRuleUntil (isJust . (Map.!? \"shiny gold\") . snd) exampleRuleBook (exampleRules !! 5) ->> exampleExpandedRules !! 5" $
        ((reduceRuleUntil (isJust . (Map.!? ("shiny gold" :: BagDescription)) . snd) exampleRulebook) =<< (exampleRules `atMay` 5)) `shouldBe` (exampleExpandedRules `atMay` 5)
      it "reduceRuleUntil (isJust . (Map.!? \"shiny gold\") . snd) exampleRuleBook (exampleRules !! 6) ->> exampleExpandedRules !! 6" $
        ((reduceRuleUntil (isJust . (Map.!? ("shiny gold" :: BagDescription)) . snd) exampleRulebook) =<< (exampleRules `atMay` 6)) `shouldBe` (exampleExpandedRules `atMay` 6)
      it "reduceRuleUntil (isJust . (Map.!? \"shiny gold\") . snd) exampleRuleBook (exampleRules !! 7) ->> exampleExpandedRules !! 7" $
        ((reduceRuleUntil (isJust . (Map.!? ("shiny gold" :: BagDescription)) . snd) exampleRulebook) =<< (exampleRules `atMay` 7)) `shouldBe` (exampleExpandedRules `atMay` 7)
      it "reduceRuleUntil (isJust . (Map.!? \"shiny gold\") . snd) exampleRuleBook (exampleRules !! 8) ->> exampleExpandedRules !! 8" $
        ((reduceRuleUntil (isJust . (Map.!? ("shiny gold" :: BagDescription)) . snd) exampleRulebook) =<< (exampleRules `atMay` 8)) `shouldBe` (exampleExpandedRules `atMay` 8)
  describe "countRulesWithBags :: BagDescription -> RuleBook -> Maybe Int" $ do
    context "Given Testcases" $ do
      it "countRulesWithBags \"shiny gold\" exampleRulebook ->> Just 4" $
        countRulesWithBags "shiny gold" exampleRulebook `shouldBe` Just 4
  describe "task2" $ do
    context "Given Testcases" $ do
      it "task2 exampleRulesString ->> Just 32" $
        task2 exampleRulesString `shouldBe` Just 32



exampleRulesString :: String
exampleRulesString = "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags."

exampleRawRules :: [RawRule]
exampleRawRules = lines exampleRulesString

exampleRules :: [Rule]
exampleRules = [("light red", Map.fromList [("bright white",1),
                                            ("muted yellow",2)]),
                 ("dark orange", Map.fromList [("bright white",3),
                                               ("muted yellow",4)]),
                 ("bright white", Map.fromList [("shiny gold", 1)]),
                 ("muted yellow", Map.fromList [("shiny gold", 2),
                                                ("faded blue", 9)]),
                 ("shiny gold", Map.fromList [("dark olive",1),
                                               ("vibrant plum",2)]),
                 ("dark olive", Map.fromList [("faded blue",3),
                                              ("dotted black",4)]),
                 ("vibrant plum", Map.fromList [("faded blue",5),
                                                ("dotted black",6)]),
                 ("faded blue", Map.empty ),
                 ("dotted black", Map.empty)]


exampleRulebook :: RuleBook
exampleRulebook = Map.fromList exampleRules

exampleFullyExpandedRules :: [Rule]
exampleFullyExpandedRules = [("light red", Map.fromList [("faded blue", 1 * (2 * 5 + 1 * 3) + 2 * (2 * (2 * 5 + 1 * 3) + 9)),
                                                     ("dotted black",1 * (2 * 6 + 1 * 4) + 2 * (2 * (2 * 6 + 1 * 4)))]),
                        ("dark orange", Map.fromList [("faded blue", (3 * (2 * 5 + 1 * 3) + 4 * (2 * (2 * 5 + 1 * 3) + 9))),
                                                     ("dotted black", 3 * (2 * 6 + 1 * 4) + 4 * (2 * (2 * 6 + 1 * 4)))]),
                        ("bright white", Map.fromList [("faded blue", 2 * 5 + 1 * 3),
                                                     ("dotted black", 2 * 6 + 1 * 4)]),
                        ("muted yellow", Map.fromList [("faded blue",2 * (1 * 3 + 2 * 5) + 9),
                                                       ("dotted black", 2 * (1 * 4 + 2 * 6))]),
                        ("shiny gold", Map.fromList [("faded blue", 2 * 5 + 1 * 3),
                                                     ("dotted black", 2 * 6 + 1 * 4)]),
                        ("dark olive", Map.fromList [("faded blue", 3),
                                                     ("dotted black", 4)]),
                        ("vibrant plum", Map.fromList [("faded blue", 5),
                                                       ("dotted black", 6)]),
                        ("faded blue", Map.empty ),
                        ("dotted black", Map.empty)]


exampleExpandedRules :: [Rule]
exampleExpandedRules = [("light red", Map.fromList [("shiny gold", 1 + 2 * 2),
                                                         ("faded blue", 18)]),
                             ("dark orange", Map.fromList [("shiny gold", 3 + 4 * 2),
                                                           ("faded blue", 4 * 9)]),
                             ("bright white", Map.fromList [("shiny gold", 1)]),
                             ("muted yellow", Map.fromList [("shiny gold", 2),
                                                            ("faded blue", 9)]),
                             ("shiny gold", Map.fromList [("dark olive", 1),
                                                          ("vibrant plum",2)]),
                             ("dark olive", Map.fromList [("faded blue",3),
                                                          ("dotted black",4)]),
                             ("vibrant plum", Map.fromList [("faded blue",5),
                                                            ("dotted black",6)]),
                             ("faded blue", Map.empty),
                             ("dotted black", Map.empty)]
