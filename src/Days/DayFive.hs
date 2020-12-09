module Days.DayFive
  ( parta
  , partb )
  where

import Data.List

import Types

getSeat :: [AirPlaneSeatType] -> Int -> Int -> Int -> Int -> Int
getSeat [] r _ c _ = 8 * r + c
getSeat (h:rest) bottom top left right =
  let midv = ((top - bottom) `div` 2) + bottom
      midh = ((right - left) `div` 2) + left
  in case h of
       Front -> getSeat rest bottom midv left right
       Back -> getSeat rest (midv + 1) top left right
       LeftSeat -> getSeat rest bottom top left midh
       RightSeat -> getSeat rest bottom top (midh + 1) right

missingSeat :: [Int] -> Int -> Int
missingSeat [] _ = error "No missing seat"
missingSeat (h:rest) l = if (h - l) /= 1 then l + 1 else missingSeat rest h

parta :: [Seating] -> Int
parta l = maximum $ map (\s -> getSeat (seat s) 0 127 0 7) l

partb :: [Seating] -> Int
partb l =
  let (h:restSeats) = sort $ map (\s -> getSeat (seat s) 0 127 0 7) l
  in missingSeat restSeats h
