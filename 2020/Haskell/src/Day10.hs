module Day10 where

import Data.Functor.Foldable (ListF (Cons, Nil), Recursive (para))
import Data.List.Extra (sort)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Lib (freqs, linedNums)

part1 :: [Int] -> Maybe Int
part1 = ans . freqs . map (uncurry subtract) . (zip <*> tail)
  where
    ans input = (*) <$> Map.lookup 1 input <*> Map.lookup 3 input

part2 :: [Int] -> Maybe Integer
part2 = listToMaybe . para f
  where
    f Nil = []
    f (Cons x (xs, res)) = g x res xs : res
      where
        g x vals = max 1 . sum . zipWith const vals . takeWhile ((<= 3) . subtract x)

main :: IO ()
main = do
    input <- sort . (\x -> x ++ [maximum x + 3]) . (0 :) . linedNums <$> readFile "../data/day10.in"
    print $ part1 input
    print $ part2 input

-- Just 2030
-- Just 42313823813632
