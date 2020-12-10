module Day10 where

import Data.List.Extra ( sort )
import qualified Data.Map as Map
import Data.Maybe ( listToMaybe )
import Lib ( freqs, linedNums )

part1 :: [Int] -> Maybe Int
part1 = ans . freqs . map (uncurry subtract) . (zip <*> tail)
  where
    ans input = (*) <$> Map.lookup 1 input <*> Map.lookup 3 input

run :: [Int] -> [(Int, Integer)]
run [] = []
run (x : xs) =
    let rec = run xs
        v = max (f x rec) 1
     in (x, v) : rec
  where
    f x = sum . map snd . filter ((<= 3) . subtract x . fst) . take 3

part2 :: [Int] -> Maybe Integer
part2 = fmap snd . listToMaybe . run

main :: IO ()
main = do
    input <- sort . (\x -> x ++ [maximum x + 3]) . (0 :) . linedNums <$> readFile "../data/day10.in"
    print $ part1 input
    print $ part2 input

-- print $ part2 input
