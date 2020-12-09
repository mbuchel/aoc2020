module Types.DaySeven where

import Data.List.Split

data Bag = Bag { bagColor :: String
               , internalBags :: [(Int, String)]
               }
  deriving (Show, Eq, Ord)

instance Read Bag where
 readsPrec _ input =
   let wordlist = words input
       bagSplit = splitWhen (== "contain") wordlist
       color = (unwords . init . head) bagSplit
       dataString = (unwords . head . tail) bagSplit
       dataSplitString = map words $ splitWhen (== ',') dataString
       valueList = map readTuple dataSplitString
   in [(Bag color valueList, "")]
   where readTuple (a:b:c:_)
           | a /= "no" = (read a :: Int, unwords [b, c])
           | otherwise = (-1, "empty bag")
         readTuple _ = (-1, "Error in parsing")
