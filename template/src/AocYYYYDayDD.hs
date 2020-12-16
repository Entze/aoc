module AocYYYYDayDD where

import qualified Data.Text as Text
import AocCommon

aocYYYYDayDDMain :: IO ()
aocYYYYDayDDMain = aocMain' instanceFromText instanceToText solve1 solution1ToText solve2 solution2ToText

type ProblemInstance = Int
type Solution1 = Int
type Solution2 = Int


instanceFromText :: Text.Text -> ELM ProblemInstance
instanceFromText  = return . read . Text.unpack

solve1 :: ProblemInstance -> ELM Solution1
solve1 = undefined

solve2 :: ProblemInstance -> ELM Solution2
solve2 = undefined

instanceToText :: ProblemInstance -> Text.Text
instanceToText = (Text.pack . show)

solution1ToText :: Solution1 -> Text.Text
solution1ToText = (Text.pack . show)

solution2ToText :: Solution2 -> Text.Text
solution2ToText = (Text.pack . show)
