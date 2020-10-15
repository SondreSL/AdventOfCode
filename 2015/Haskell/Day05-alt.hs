module Main where

import           Data.List                      ( isInfixOf )
import           Control.Monad
import           Data.Maybe
import           Data.Monoid

allPass :: [a -> Bool] -> a -> Bool
allPass ps = getAll . foldMap (All .) ps

part1 :: [String] -> Int
part1 = length . filter (allPass [bad, row, threeVowels])
 where
  count f = length . filter f
  bad         = not . flip any ["ab", "cd", "pq", "xy"] . flip isInfixOf
  row         = any (uncurry (==)) . (zip <*> tail)
  threeVowels = (3 <=) . count (`elem` "aeiou")

part2 :: [String] -> Int
part2 = length . filter (allPass [twicePair, oneSep])
 where
  twicePair = or . twicePair'
  twicePair' (x : y : xs) = isInfixOf [x, y] xs : twicePair' (y : xs)
  twicePair' xs           = [False]
  oneSep = or . oneSep'
  oneSep' (x : y : z : xs) = (x == z) : oneSep' (y : z : xs)
  oneSep' xs               = [False]

main :: IO ()
main = do
  input <- lines <$> readFile "../data/05.in"
  print $ part1 input
  print $ part2 input

-- 255
-- 55
