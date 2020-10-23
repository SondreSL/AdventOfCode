module Main where

import           Data.List                      ( isInfixOf )
import Control.Monad
import Data.Maybe

count :: Ord a => (a -> Bool) -> [a] -> Int
count f = length . filter f

justIf :: (a -> Bool) -> a -> Maybe a
justIf p a = if p a then Just a else Nothing

part1 :: [String] -> Int
part1 = length . mapMaybe (justIf bad >=> row >=> threeVowels)
 where
  bad str = not $ any (`isInfixOf` str) ["ab", "cd", "pq", "xy"]
  row         = justIf $ any (uncurry (==)) . (zip <*> tail)
  threeVowels = justIf $ (3 <=) . count (`elem` "aeiou")

part2 :: [String] -> Int
part2 = length . mapMaybe (justIf twicePair >=> justIf oneSep)
  where
    twicePair = or . twicePair'
    twicePair' (x:y:xs) = isInfixOf [x,y] xs : twicePair' (y:xs)
    twicePair' xs = [False]
    oneSep = or . oneSep'
    oneSep' (x : y : z : xs) = (x == z) : oneSep' (y : z : xs)
    oneSep' xs = [False]

main :: IO ()
main = do
  input <- lines <$> readFile "../data/05.in"
  print $ part1 input
  print $ part2 input

-- 255
-- 55
