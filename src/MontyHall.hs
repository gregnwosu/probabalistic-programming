{-# LANGUAGE RankNTypes #-}
module MontyHall where
import Data.List ((\\))
import Lib
import Text.Printf (printf)
import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Distribution ((??), (?=<<))
import Control.Monad

data Outcome = Win | Lose deriving (Ord, Eq, Show)
--type Trans a  = a -> Dist.T  a
-- the probability distribution before any doors are opened
firstChoice' :: (Fractional prob) => Dist.T prob Outcome
firstChoice' = Dist.uniform [Win, Lose, Lose]
-- this is a transition , it maps a possible outcome to a new distrbutiokn of the same type of outcome

switch' Win  = Dist.certainly Lose
switch' Lose = Dist.certainly Win
printDist :: (Show a , Show p) => Dist.T p a -> String
printDist = (   ("\n\t"++) . show . Dist.decons)



data Door = A | B | C deriving (Enum,Eq,Show)
doors :: [Door]
doors = [A .. C]

data State = Doors {prize :: Door , chosen :: Door , opened :: Door}
type Strategy = forall p. (Fractional p) => State -> Dist.T p State
start :: State
start  = Doors {prize = undefined, chosen = undefined, opened =undefined}
-- hide the prize
hide :: Strategy
hide s = Dist.uniform [s {prize=d}| d <- doors]

choose :: Strategy
choose s = Dist.uniform [s {chosen=d} | d <- doors]

open :: Strategy
open s = Dist.uniform [s {chosen=d} | d <- doors]

switch  :: Strategy
switch s = Dist.uniform [s{chosen=d} | d <- doors \\ [chosen s,opened s]]

stay :: Strategy
stay = certainlyT   id

certainlyT :: (Fractional p ) => (a -> a ) -> a -> Dist.T p a
certainlyT f = Dist.certainly . f

game :: Fractional p => Strategy -> State -> Dist.T p State
game s = sequ [hide, choose,open, s]

result ::  State ->  Outcome
result s = if (chosen s == prize s) then Win else Lose

eval :: Fractional p => Strategy -> Dist.T p Outcome
eval s =  result <$> (game s start)


main' = do
  putStrLn "\n outcome for firstChoice:"
  putStrLn $ printDist    firstChoice'
  putStrLn "\n outcome for switching doors:"
  putStrLn $ printDist $  firstChoice' >>= switch'

  putStrLn "eval monty hall: switch"
  putStrLn . printDist $ eval stay
  putStrLn "eval monty hall: stay"
  putStrLn . printDist $ eval stay
