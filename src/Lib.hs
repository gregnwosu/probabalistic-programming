{-# LANGUAGE RankNTypes #-}
module Lib where

import qualified Numeric.Probability.Distribution as Dist
import Control.Monad.Trans.State
import Control.Monad (replicateM, sequence, (>=>))
import Data.List (delete)
import Text.Printf

-- from  http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf
type Trans p a = p -> Dist.T p a

prob :: (Num prob ) => Trans prob Bool
prob p = Dist.choose p True False

removeEach xs = foldr go [] xs
      where go a = (:) (a, delete a xs)
-- helper function to select an item from a list for probability measurements
selectOne :: (Fractional prob, Eq a) => StateT [a] (Dist.T prob) a
selectOne = StateT $ Dist.uniform . removeEach

select ::  (Fractional prob, Eq a) => Int -> [a] -> Dist.T prob [a]
select n = evalStateT $ replicateM n selectOne

toPct :: Double -> String
toPct = printf "%.2f%%" . (10*)

joinWith :: (Num prob) =>  (a -> b -> c) -> Dist.T prob a -> Dist.T prob b -> Dist.T prob c
joinWith  f (Dist.Cons d) (Dist.Cons d') = Dist.Cons [ (f x y, p * q) | (x,p) <- d, (y,q) <- d']

(>@>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >@> g = (>>= g) . f

sequ :: Monad m => [a -> m a] -> a -> m a
sequ = foldl (>=>) return
