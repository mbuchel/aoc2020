module Days.DaySix
  ( parta
  , partb )
  where

import Data.List

import Types

parta :: [[Person]] -> Int
parta people =
  let groupVotes = map (map votes) people
      combinedVotes = map (concat) groupVotes
      uniqueVotes = map nub combinedVotes
      numberOfVotes = map length uniqueVotes
  in sum numberOfVotes

partb :: [[Person]] -> Int
partb people =
  let groupVotes = map (map votes) people
      commonLists = map (foldl common alphabet) groupVotes
      commonVotes = map length commonLists
  in sum commonVotes
  where common x y = [i | i <- x, j <- y, i == j]
        alphabet = "abcdefghijklmnopqrstuvwxyz"
