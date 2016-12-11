module PlayingCardsExample where
import Lib
import  Numeric.Probability.Distribution
import Text.Printf

data Suit = Club | Spade | Heart |Diamond
          deriving (Eq, Ord , Show , Enum)

data Rank = Plain Int | Jack | Queen | King | Ace
          deriving (Eq,Ord , Show)

type Card = (Rank, Suit)

plains :: [Rank]
plains = Plain <$> [2..10]

faces :: [Rank]
faces = [Jack,Queen, King, Ace]

isFace :: Card -> Bool
isFace (r, _ ) = r `elem` faces

isPlain :: Card -> Bool
isPlain (Plain _, _) = True
isPlain _ = False

ranks :: [Rank]
ranks = plains ++ faces

suits :: [Suit]
suits = [Club, Spade, Heart, Diamond]

deck :: [Card]
deck = [(r,s) | r <- ranks , s <- suits]
-- helper function that enumerates a list with an element deleted
-- removeEach [1..10]

--[(1, [2,3,4,5,6,7,8,9,10]),
-- (2, [1,3,4,5,6,7,8,9,10]),
-- (3, [1,2,4,5,6,7,8,9,10]),
-- (4, [1,2,3,5,6,7,8,9,10]),
-- (5, [1,2,3,4,6,7,8,9,10]),
-- (6, [1,2,3,4,5,7,8,9,10]),
-- (7, [1,2,3,4,5,6,8,9,10]),
-- (8, [1,2,3,4,5,6,7,9,10]),
-- (9, [1,2,3,4,5,6,7,8,10]),
-- (10,[1,2,3,4,5,6,7,8,9])]
