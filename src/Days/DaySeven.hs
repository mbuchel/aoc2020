module Days.DaySeven
  ( parta
  , partb )
  where

import qualified Data.Set as S

import Types

fixBagList :: [Bag] -> [Bag]
fixBagList = map (\x -> if (internalBags x) == [(-1, "empty bag")]
                                then x { internalBags = [] }
                                else x)

countBags :: String -> [Bag] -> Int
countBags color initialList =
  let currentBagList = filter (\x -> bagColor x == color) initialList
      currentBag = head currentBagList
      bagList = internalBags currentBag
      initialSum = foldl (\a x -> a + (fst x)) 0 bagList
      recursiveSum = sum $ map (\x -> (fst x) * (countBags (snd x) initialList)) bagList
  in if bagList == []
        then 0
        else initialSum + recursiveSum

holdingBags :: String -> S.Set Bag -> S.Set Bag
holdingBags color initialSet =
  let directSet = S.filter bagContains initialSet
      remainingSet = S.difference initialSet directSet
      remainingColors = map bagColor $ S.toList directSet
      indirectSet = S.unions $ map (\x -> holdingBags x remainingSet) remainingColors
  in if null directSet
        then directSet
        else S.union directSet indirectSet
  where bagContains s = (length $ filter (\x -> snd x == color) (internalBags s)) /= 0

parta :: [Bag] -> Int
parta = S.size . (holdingBags "shiny gold") . S.fromList . fixBagList

partb :: [Bag] -> Int
partb = (countBags "shiny gold") . fixBagList
