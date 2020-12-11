module Days
  ( day1
  , day2
  , day3
  , day4
  , day5
  , day6
  , day7
  , day8
  , day9
  , day10 )
  where

import  Types
import  Utils

import qualified Days.DayOne as One
import qualified Days.DayTwo as Two
import qualified Days.DayThree as Three
import qualified Days.DayFour as Four
import qualified Days.DayFive as Five
import qualified Days.DaySix as Six
import qualified Days.DaySeven as Seven
import qualified Days.DayEight as Eight
import qualified Days.DayNine as Nine
import qualified Days.DayTen as Ten

day1 =
  [ ("day1/input", One.parta, readIntList)
  , ("day1/input", One.partb, readIntList)
  ]

day2 =
  [ ("day2/input", Two.parta, readPassList)
  , ("day2/input", Two.partb, readPassList)
  ]

day3 =
  [ ("day3/input", Three.parta, readGeology)
  , ("day3/input", Three.partb, readGeology)
  ]

day4 =
  [ ("day4/input", Four.parta, readAoCPassports)
  , ("day4/input", Four.partb, readAoCPassports)
  ]

day5 =
  [ ("day5/input", Five.parta, readSeatings)
  , ("day5/input", Five.partb, readSeatings)
  ]

day6 =
  [ ("day6/input", Six.parta, readAirplaneVotes)
  , ("day6/input", Six.partb, readAirplaneVotes)
  ]

day7 =
  [ ("day7/input", Seven.parta, readAirplaneBags)
  , ("day7/input", Seven.partb, readAirplaneBags)
  ]

day8 =
  [ ("day8/input", Eight.parta, readHandheldOps)
  , ("day8/input", Eight.partb, readHandheldOps)
  ]

day9 =
  [ ("day9/input", Nine.parta, readIntList)
  , ("day9/input", Nine.partb, readIntList)
  ]

day10 =
  [ ("day10/input", Ten.parta, readIntList)
  , ("day10/input", Ten.partb, readIntList) -- Takes too long to calculate
  ]

readHandheldOps :: String -> IO [HandheldOps]
readHandheldOps = readAoCList

readAirplaneBags :: String -> IO [Bag]
readAirplaneBags = readAoCList

readAirplaneVotes :: String -> IO [[Person]]
readAirplaneVotes = readAoCGroups

readIntList :: String -> IO [Int]
readIntList = readAoCList

readPassList :: String -> IO [Password]
readPassList = readAoCList

readGeology :: String -> IO [GeoLine]
readGeology = readAoCList

readSeatings :: String -> IO [Seating]
readSeatings = readAoCList
