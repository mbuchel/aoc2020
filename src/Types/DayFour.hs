{-# LANGUAGE DataKinds #-}
module Types.DayFour where

import Data.Char

data Height = Cm { inCm :: Int }
                   | In { inIn :: Int }
                   | InvalidHeight
        deriving (Show, Eq)

data PassportFields = Byr { birthYear :: Int }
                                | Iyr { issueYear :: Int }
                                | Eyr { expiryYear :: Int }
                                | Hgt { height :: Height }
                               | Hcl { hairColor :: String }
                               | Ecl { eyeColor :: String }
                               | Pid { passportId :: String }
                               | Cid { countryId :: Int }
        deriving (Show, Eq)

data Passport =
  Passport { byr :: Maybe PassportFields
           , iyr :: Maybe PassportFields
           , eyr :: Maybe PassportFields
           , hgt :: Maybe PassportFields
           , hcl :: Maybe PassportFields
           , ecl :: Maybe PassportFields
           , pid :: Maybe PassportFields
           , cid :: Maybe PassportFields }
  deriving Show

instance Read PassportFields where
  readsPrec _ input =
    let (curr:restTmp) = words input
        rest = unwords restTmp
        (x:y:z:c:val) = curr
        field = [x, y, z]
        h = read (take ((length val) - 2) val) :: Int
        m = drop ((length val) - 2) val
        heightVal = case m of
                                "cm" -> Cm h
                                "in" -> In h
                                _  -> InvalidHeight
        (pidVal, _) = span isDigit val
    in if c == ':'
          then case field of
                 "byr" -> [(Byr (read val :: Int), rest)]
                 "iyr" -> [(Iyr (read val :: Int), rest)]
                 "eyr" -> [(Eyr (read val :: Int), rest)]
                 "hgt" -> [(Hgt heightVal, rest)]
                 "hcl" -> [(Hcl val, rest)]
                 "ecl" -> [(Ecl val, rest)]
                 "pid" -> [(Pid pidVal, rest)]
                 "cid" -> [(Cid (read val :: Int), rest)]
                 _ -> []
          else []

modifyPassport :: Maybe PassportFields -> Passport -> Passport
modifyPassport (Just f) p = case f of
                                        Byr _ -> p { byr = Just f }
                                        Iyr _ -> p { iyr = Just f }
                                        Eyr _ -> p { eyr = Just f }
                                        Hgt _ -> p { hgt = Just f }
                                        Hcl _ -> p { hcl = Just f }
                                        Ecl _ -> p { ecl = Just f }
                                        Pid _ -> p { pid = Just f }
                                        Cid _ -> p { cid = Just f }
modifyPassport _ p = p

makePassport :: [Maybe PassportFields] -> Passport
makePassport = foldl (\x y -> modifyPassport y x) nullPassport
  where nullPassport = Passport Nothing
                                                  Nothing
                                                  Nothing
                                                  Nothing
                                                  Nothing
                                                  Nothing
                                                  Nothing
                                                  Nothing
