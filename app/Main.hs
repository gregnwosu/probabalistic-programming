module Main where

import Numeric.Probability.Distribution ((??), (?=<<))
import qualified Numeric.Probability.Distribution as Dist
import Lib

main :: IO ()
main = do
  print $ eVal ?? dVal ?=<< bNetwork
  print $ Dist.just [(Plain 3, Heart), (Plain 3, Diamond)] ?? select 2 deck
