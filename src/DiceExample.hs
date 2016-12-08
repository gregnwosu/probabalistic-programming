module DiceExample where
import Lib
import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Distribution ((??), (?=<<))

die :: (Fractional prob) => Dist.T prob Int
die = Dist.uniform [1..6]

dice :: (Fractional prob)  => Integer -> Dist.T prob [Int]
dice 0 = Dist.certainly []
dice n = joinWith (:) die (dice (n - 1))

-- What is the probabolity of at least 2 sixes whe  throwing 4 dies
puzzle1 :: String
puzzle1 = toPct $((>=2) . length . filter (==6)) Dist.?? dice 4
-- “What is the probability of drawing a red, green, and blue marble (in this order) from a jar containing two red, two green, and one blue marble without putting them back?”
data Colours = R | G | B
             deriving (Eq)
puzzle2 = toPct $ (== [R,G,B]) Dist.?? select 3 [R,R,G,G,B]
