module Days.DayNine
  ( findInvalid
  , contagiousList
  , parta
  , partb )
  where

preambleValidTill :: [Int] -> [Int] -> Int
preambleValidTill _ [] = error "Invalid number not found"
preambleValidTill preamble (current:remaining) =
  let validAnswers = [(x, y) | x <- preamble, y <- preamble, x + y == current, x /= y]
      newpreamble = tail preamble ++ [current]
  in if validAnswers == []
        then current
        else preambleValidTill newpreamble remaining

findInvalid :: [Int] -> Int -> Int
findInvalid list preambleLength =
  let (preamble, realList) = splitAt preambleLength list
  in preambleValidTill preamble realList

contagiousList :: Int -> Int -> [Int] -> [Int]
contagiousList n y l =
  let contagious = take n l
      summedValue = sum contagious
  in if summedValue == y
        then contagious
        else if summedValue > y
                then contagiousList 2 y (tail l)
                else contagiousList (n + 1) y l

parta :: [Int] -> Int
parta values = findInvalid values 25

partb :: [Int] -> Int
partb values =
  let invalid = findInvalid values 25
      contagious = contagiousList 2 invalid values
  in (minimum contagious) + (maximum contagious)
