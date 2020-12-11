module Utils
  ( readAoCList
  , readAoCPassports
  , readAoCGroups
  , dropEveryN
  , dropIndices
  , keepEveryN )
  where

import Data.List.Split
import Data.Maybe
import Text.Read

import qualified Data.Set as S

import Types

readAoCList :: (Read a) => String -> IO [a]
readAoCList file = map read . lines <$> readFile file

readAoCPassports :: String -> IO [Passport]
readAoCPassports file = do
  input <- map words . lines <$> readFile file
  let tempFields = splitWhen null input
  let fields = map concat tempFields
  let passportFields = map (map (\x -> (readMaybe x :: Maybe PassportFields))) fields
  return $ map makePassport passportFields

readAoCGroups :: (Read a, Eq a) => String -> IO [[a]]
readAoCGroups file = do
  input <- map readMaybe . lines <$> readFile file
  let groupings = splitWhen (== Nothing) input
  return $ map (map fromJust) groupings

dropIndices :: S.Set Int -> [a] -> [a]
dropIndices indices = foldr step [] . zip [1..]
  where step (i, x) acc = if (S.member i indices) then acc else x:acc

dropEveryN :: Int -> [a] -> [a]
dropEveryN n
  | n == 1 = id
  | otherwise = foldr step [] . zip [1..]
  where step (i, x) acc = if (i `mod` n) == 0 then acc else x:acc

keepEveryN :: Int -> [a] -> [a]
keepEveryN n
  | n == 1 = id
  | otherwise = foldr step [] . zip [0..]
  where step (i, x) acc = if (i `mod` n) == 0 then x:acc else acc
