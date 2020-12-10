module Day10 where

import Data.Functor.Foldable
import Data.List.Extra (sort)
import Data.Maybe (listToMaybe)
import Lib (freqs, linedNums, zipWithTail)
import qualified Data.Map as Map

part1 :: [Int] -> Maybe Int
part1 = adapterProduct . freqs . map (uncurry subtract) . zipWithTail
  where
    adapterProduct input = (*) <$> Map.lookup 1 input <*> Map.lookup 3 input

part2 :: [Int] -> Maybe Integer
part2 = listToMaybe . para f
  where
    f Nil = []
    f (Cons x (xs, res)) = next : res
      where
        next = max 1 . sum . zipWith const res . takeWhile (<= x + 3) $ xs

main :: IO ()
main = do
    input <- sort . (\xs -> maximum xs + 3 : 0 : xs) . linedNums <$> readFile "../data/day10.in"
    print $ part1 input
    print $ part2 input

-- Just 2030
-- Just 42313823813632
