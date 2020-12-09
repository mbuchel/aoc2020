module Days.DayThree
  ( parta
  , partb )
  where

import Types
import Utils

treesHitOnSlope :: Int -> Int -> [GeoLine] -> Int
treesHitOnSlope xslope yslope initGeology =
  let geology = dropEveryN yslope initGeology
      indices = map (\x -> xslope * (x - 1)) [1..]
      path = zipWith (\a b -> (line a) !! b) geology indices
  in length $ filter (== Tree) path

parta :: [GeoLine] -> Int
parta = treesHitOnSlope 3 1

partb :: [GeoLine] -> Int
partb geology =
  let slopeOne = treesHitOnSlope 1 1 geology
      slopeThree = treesHitOnSlope 3 1 geology
      slopeFive = treesHitOnSlope 5 1 geology
      slopeSeven = treesHitOnSlope 7 1 geology
      slopeTwoDown = treesHitOnSlope 1 2 geology
  in slopeOne * slopeThree * slopeFive * slopeSeven * slopeTwoDown
