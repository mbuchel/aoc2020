module Days.DayTen
  ( parta
  , partb )
  where

import Data.List
import qualified Data.Set as S

import Utils

checkValid :: [Int] -> Bool
checkValid values =
  let joltDiff = zipWith (\x y -> x - y) (tail values) values
  in 0 == (length $ filter (\x -> x < 1 || x > 3) joltDiff)

parta :: [Int] -> Int
parta values =
  let newValues = values ++ [0, (maximum values) + 3]
      sorted = sort newValues
      joltDiff = zipWith (\x y -> x - y) (tail sorted) sorted
      oneLen = length $ filter (== 1) joltDiff
      threeLen = length $ filter (== 3) joltDiff
  in oneLen * threeLen

partb :: [Int] -> Int
partb values =
  let allPossible = S.powerSet alphabet
      fullMap = S.filter (\x -> checkValid (dropIndices x vals)) allPossible
  in S.size fullMap
  where vals = sort $ values ++ [0, (maximum values) + 3]
        validRemovals = [S.fromList [i] | i <- [2..(length vals - 1)], checkValid (take (i - 1) vals ++ drop (i) vals)]
        alphabet = S.unions validRemovals
