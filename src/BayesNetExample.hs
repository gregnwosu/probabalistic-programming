module BayesNetExample  where
import Lib
import  Numeric.Probability.Distribution ((??),(?=<<))

-- the following is a representation of the baysian net we will produce with code
--
-- digraph dot {
-- a -> c
-- b -> c
-- c -> d
-- c -> e
-- }
-- prob A P(A)
a :: Dist Bool
a = prob 0.2

b :: Dist Bool
b = prob 0.05

c :: Bool -> Trans Bool
c False False = prob 0.9
c False True = prob 0.5
c True False = prob 0.3
c True True = prob 0.1

d :: Trans Bool
d False = prob 0.1
d True = prob 0.4

e :: Trans Bool
e False = prob 0.5
e True = prob 0.2

data Network = N {aVal :: Bool , bVal :: Bool, cVal :: Bool, dVal :: Bool, eVal :: Bool} deriving (Eq, Ord , Show)

bNetwork :: Dist Network
bNetwork = do a' <- a
              b' <- b
              c' <- c a' b'
              d' <- d c'
              e' <- e c'
              return (N a' b' c' d' e')
-- “main = print $ eVal ?? dVal ?=<< bNetwork”
-- Excerpt From: Shukla, Nishant. “Haskell Data Analysis Cookbook.” iBook
