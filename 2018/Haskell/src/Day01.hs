module Day01 where

import Data.Set (Set)
import qualified Data.Set as Set

parse :: String -> [Int]
parse = map read . lines . filter (/= '+')

firstRepeat :: Set Int -> [Int] -> Int
firstRepeat _ [] = undefined
firstRepeat seen (x : xs) =
  if Set.member x seen
    then x
    else firstRepeat (Set.insert x seen) xs

main :: IO ()
main = do
  input <- parse <$> readFile "../data/day01.in"
  print $ sum input
  print $ firstRepeat Set.empty . scanl (+) 0 $ cycle input

-- 533
-- 73272
