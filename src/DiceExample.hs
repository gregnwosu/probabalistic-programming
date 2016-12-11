module DiceExample where
import Lib
import Numeric.Probability.Distribution hiding (filter, select)

die :: Dist Integer
die = uniform [1..6]

dice :: Integer -> Dist [Integer]
dice 0 = certainly []
dice n = joinWith (:) die (dice (n - 1))

-- What is the probability of at least 2 sixes whe  throwing 4 dies
puzzle1 :: String
puzzle1 = toPct . fromRational $((>=2) . length . filter (==6)) ?? dice 4
-- “What is the probability of drawing a red, green, and blue marble (in this order) from a jar containing two red, two green, and one blue marble without putting them back?”
data Colours = R | G | B
             deriving (Eq)
puzzle2 = toPct .  fromRational $ (== [R,G,B]) ?? select 3 [R,R,G,G,B]
