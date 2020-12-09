module Days.DayTwo
  ( parta
  , partb )
  where

import Types

import qualified Data.Text as T

parta :: [Password] -> Int
parta passwords = length $ filter
  (\x -> let counted = T.count (findChar x) (password x)
             minNum = minNumber x
             maxNum = maxNumber x
         in counted >= minNum && maxNum >= counted)
  passwords

partb :: [Password] -> Int
partb passwords = length $ filter
  (\x -> let minNum = minNumber x
             maxNum = maxNumber x
             pass = password x
             minVal = T.index pass (minNum - 1)
             maxVal = T.index pass (maxNum - 1)
             check = T.index (findChar x) 0
         in (minVal == check && maxVal /= check) ||
            (minVal /= check && maxVal == check))
  passwords
