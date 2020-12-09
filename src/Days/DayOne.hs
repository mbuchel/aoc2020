module Days.DayOne
  ( parta
  , partb )
  where

parta :: [Int] -> Int
parta ints = do
  let combinations = [(x, y) | x <- ints, y <- ints, x + y == 2020]
  let (x, y) = head combinations
  x * y

partb :: [Int] -> Int
partb ints = do
  let combinations = [(x, y, z) | x <- ints, y <- ints, z <- ints, x + y + z == 2020]
  let (x, y, z) = head combinations
  x * y * z
