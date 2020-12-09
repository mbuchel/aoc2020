module Types.DayTwo where

import Data.Char

import qualified Data.Text as T

data Password = Password
  { minNumber :: Int
  , maxNumber :: Int
  , findChar  :: T.Text
  , password  :: T.Text
  } deriving Show

instance Read Password where
  readsPrec _ input =
    let (minNumberStr, rest1) = span isDigit input
        minNum = read minNumberStr :: Int
        (t0:rest2) = rest1
        (maxNumberStr, rest3) = span isDigit rest2
        maxNum = read maxNumberStr :: Int
        (t1:findCharacter:t2:t3:passwordStr) = rest3
        pass = T.pack passwordStr
        char = T.pack [findCharacter]
    in
      if t0 == '-' && t1 == ' ' && t2 == ':' && t3 == ' '
        then [(Password minNum maxNum char pass, "")]
        else []
