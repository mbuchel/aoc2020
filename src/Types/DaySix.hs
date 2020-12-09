module Types.DaySix where

import Data.Char
import Data.List

data Person = Person { votes :: [Char] }
  deriving (Show, Eq)

instance Read Person where
  readsPrec _ input =
    let (votingStr, rest) = span isAlpha input
        votings = nub votingStr
    in if votings /= ""
          then [(Person votings, rest)]
          else []
