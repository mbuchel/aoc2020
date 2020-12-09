module Days.DayFour
  ( parta
  , partb )
  where

import Types

import Data.List

checkHairColor :: String -> Bool
checkHairColor s =
  let (hashtag:rest) = s
      correct = all (\x -> ('0' <= x && x <= '9') ||
                           ('a' <= x && x <= 'f')) rest
  in hashtag == '#' && correct && length rest == 6

checkEyeColor :: String -> Bool
checkEyeColor s =
  let acceptableColors = "amb blu brn gry grn hzl oth"
  in isInfixOf s acceptableColors

validPassportField :: Maybe PassportFields -> Bool
validPassportField pass = case pass of
  (Just (Byr year)) -> 1920 <= year && year <= 2002
  (Just (Iyr year)) -> 2010 <= year && year <= 2020
  (Just (Eyr year)) -> 2020 <= year && year <= 2030
  (Just (Hgt (Cm h))) -> 150 <= h && h <= 193
  (Just (Hgt (In h))) -> 59 <= h && h <= 76
  (Just (Hcl c)) -> checkHairColor c
  (Just (Ecl c)) -> checkEyeColor c
  (Just (Pid p)) -> length p == 9
  _ -> False

parta :: [Passport] -> Int
parta passports = length $ filter isValid passports
  where isValid x =
          let b = byr x
              i = iyr x
              e = eyr x
              h = hgt x
              hc = hcl x
              ec = ecl x
              p = pid x
          in b /= Nothing &&
             i /= Nothing &&
             e /= Nothing &&
             h /= Nothing &&
             hc /= Nothing &&
             ec /= Nothing &&
             p /= Nothing

partb :: [Passport] -> Int
partb passports = length $ filter isValid passports
  where isValid x =
           let b = byr x
               i = iyr x
               e = eyr x
               h = hgt x
               hc = hcl x
               ec = ecl x
               p = pid x
           in validPassportField b &&
              validPassportField i &&
              validPassportField e &&
              validPassportField h &&
              validPassportField hc &&
              validPassportField ec &&
              validPassportField p
