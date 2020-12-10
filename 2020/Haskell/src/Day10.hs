module Day10 where

import Data.Functor.Foldable ( ListF(Cons, Nil), Recursive(para) )
import Data.List.Extra (sort)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Lib (freqs, linedNums)

part1 :: [Int] -> Maybe Int
part1 = ans . freqs . map (uncurry subtract) . (zip <*> tail)
  where
    ans input = (*) <$> Map.lookup 1 input <*> Map.lookup 3 input

run :: [Int] -> [Integer]
run = para $ \case
    Nil -> []
    Cons x (xs, res) -> f x (zip xs res) : res
  where
    f x = max 1 . sum . map snd . filter ((<= 3) . subtract x . fst) . take 3

part2 :: [Int] -> Maybe Integer
part2 = listToMaybe . run

main :: IO ()
main = do
    input <- sort . (\x -> x ++ [maximum x + 3]) . (0 :) . linedNums <$> readFile "../data/day10.in"
    print $ part1 input
    print $ part2 input

-- print $ part2 input
