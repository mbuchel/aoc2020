module Main where

import Days

runAoc (day, f, parser) = do
  input <- parser $ "data/" ++ day
  putStrLn $ "For " ++ day ++ " we got: " ++ show (f input)

main :: IO ()
main = do
  mapM_ runAoc day1
  mapM_ runAoc day2
  mapM_ runAoc day3
  mapM_ runAoc day4
  mapM_ runAoc day5
  mapM_ runAoc day6
  mapM_ runAoc day7
  mapM_ runAoc day8
