{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module DaySpec (spec) where

import qualified Days.DayOne as One
import qualified Days.DayTwo as Two
import qualified Days.DayThree as Three
import qualified Days.DayFour as Four
import qualified Days.DayFive as Five
import qualified Days.DaySix as Six
import qualified Days.DaySeven as Seven
import qualified Days.DayEight as Eight
import qualified Days.DayNine as Nine

import Types
import Utils

import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "Day 1" $ do
    it "Part A" $ do
      ints <- readAoCList "data/day1/practice" :: IO [Int]
      One.parta ints `shouldBe` 514579

    it "Part B" $ do
      ints <- readAoCList "data/day1/practice" :: IO [Int]
      One.partb ints `shouldBe` 241861950

  describe "Day 2" $ do
    it "Part A" $ do
      passwords <- readAoCList "data/day2/practice" :: IO [Password]
      Two.parta passwords `shouldBe` 2

    it "Part B" $ do
      passwords <- readAoCList "data/day2/practice" :: IO [Password]
      Two.partb passwords `shouldBe` 1

  describe "Day 3" $ do
    it "Part A" $ do
      geology <- readAoCList "data/day3/practice" :: IO [GeoLine]
      Three.parta geology `shouldBe` 7

    it "Part B" $ do
      geology <- readAoCList "data/day3/practice" :: IO [GeoLine]
      Three.partb geology `shouldBe` 336

  describe "Day 4" $ do
    it "Part A" $ do
      passports  <- readAoCPassports "data/day4/practice"
      Four.parta passports `shouldBe` 2

    it "Part B" $ do
      passports  <- readAoCPassports "data/day4/practice-part2"
      Four.partb passports `shouldBe` 4

  describe "Day 5" $ do
    it "Boarding Pass #1" $ do
      let pass = read "BFFFBBFRRR" :: Seating
      Five.parta [pass] `shouldBe` 567

    it "Boarding Pass #2" $ do
      let pass = read "FFFBBBFRRR" :: Seating
      Five.parta [pass] `shouldBe` 119

    it "Boarding Pass #3" $ do
      let pass = read "BBFFBBFRLL" :: Seating
      Five.parta [pass] `shouldBe` 820

  describe "Day 6" $ do
    it "Part A" $ do
      people <- readAoCGroups "data/day6/practice" :: IO [[Person]]
      Six.parta people `shouldBe` 11

    it "Part B" $ do
      people <- readAoCGroups "data/day6/practice" :: IO [[Person]]
      Six.partb people `shouldBe` 6

  describe "Day 7" $ do
    it "Part A" $ do
      bags <- readAoCList "data/day7/practice" :: IO [Bag]
      Seven.parta bags `shouldBe` 4

    it "Part B" $ do
      bags <- readAoCList "data/day7/practice-partb" :: IO [Bag]
      Seven.partb bags `shouldBe` 126

  describe "Day 8" $ do
    it "Part A" $ do
      ops <- readAoCList "data/day8/practice" :: IO [HandheldOps]
      Eight.parta ops `shouldBe` 5

    it "Part B" $ do
      ops <- readAoCList "data/day8/practice" :: IO [HandheldOps]
      Eight.partb ops `shouldBe` 8

  describe "Day 9" $ do
    it "Part A" $ do
      ints <- readAoCList "data/day9/practice" :: IO [Int]
      Nine.findInvalid ints 5 `shouldBe` 127

    it "Part B" $ do
      ints <- readAoCList "data/day9/practice" :: IO [Int]
      let invalid = Nine.findInvalid ints 5
      invalid `shouldBe` 127
      let contagious = Nine.contagiousList 2 invalid ints
      let l = maximum contagious
      let h = minimum contagious
      l + h `shouldBe` 62
