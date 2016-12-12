{-# LANGUAGE RankNTypes #-}
module Lib where

import  Numeric.Probability.Distribution
import Control.Monad.Trans.State
import Control.Monad (replicateM, sequence, (>=>))
import Data.List (delete)
import Text.Printf

type Probability = Double
-- from  http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf
type Dist = T Probability
type Trans p = p -> Dist p

removeEach :: (Eq a) => [a] -> [(a, [a])]
removeEach xs = foldr go [] xs
      where go a = (:) (a, delete a xs)
-- helper function to select an item from a list for probability measurements
selectOne :: Eq a => StateT [a] Dist a
selectOne = StateT $ uniform . removeEach

select ::  Eq a => Int -> Trans [a]
select n = evalStateT $ replicateM n selectOne

toPct :: Double -> String
toPct = printf "%.2f%%" . (100*)

joinWith :: (Num prob) =>  (a -> b -> c) -> Dist a -> Dist b -> Dist c
joinWith  f (Cons d) (Cons d') =
    Cons [ (f x y, p * q) | (x,p) <- d, (y,q) <- d']

(>@>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >@> g = (>>= g) . f

sequ :: Monad m => [a -> m a] -> a -> m a
sequ = foldl (>=>) return

prob :: Double -> Dist Bool
prob p = choose p True False
