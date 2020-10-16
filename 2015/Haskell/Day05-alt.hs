module Main where

import           Data.List                      ( isInfixOf
                                                , tails
                                                )
import           Control.Applicative
import           Data.Maybe
import           Data.Monoid

allPass :: [a -> Bool] -> a -> Bool
allPass ps = getAll . foldMap (All .) ps

-- Filters
bad, row, threeVowels, twicePair, oneSep :: String -> Bool
bad         = not . flip any ["ab", "cd", "pq", "xy"] . flip isInfixOf
row         = any (uncurry (==)) . (zip <*> tail)
threeVowels = (3 <=) . length . filter (`elem` "aeiou")
twicePair = any (\(x:y:xs) -> [x,y] `isInfixOf` xs) . filter ((> 2) . length) . tails
oneSep    = any (\(x:y:z:xs) -> x == z) . filter ((> 2) . length) . tails

part1 :: [String] -> Int
part1 = length . filter (allPass [bad, row, threeVowels])

part1' :: [String] -> Int
part1' = length . filter (foldl1 (liftA2 (&&)) [bad, row, threeVowels])

part2 :: [String] -> Int
part2 = length . filter (allPass [twicePair, oneSep])

part2' :: [String] -> Int
part2' = length . filter (foldl (liftA2 (&&)) (const True) [twicePair, oneSep])

main :: IO ()
main = do
  input <- lines <$> readFile "../data/05.in"
  print $ part1 input
  print $ part1' input
  print $ part2 input
  print $ part2' input

-- 255
-- 55
