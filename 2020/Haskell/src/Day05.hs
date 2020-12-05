module Day05 where

import Data.List (find, sort)
import Data.Digits ( unDigits )

seat :: [Char] -> Int
seat = unDigits 2 . map f
  where f 'F' = 0
        f 'B' = 1
        f 'L' = 0
        f 'R' = 1

part1 :: [String] -> Int
part1 = maximum . map seat

part2 :: [String] -> Maybe Int
part2 =
    fmap snd
        . find (uncurry (/=))
        . (zip <*> (tail . map pred))
        . sort
        . map seat

main :: IO ()
main = do
    input <- lines <$> readFile "../data/day05.in"
    print $ part1 input
    print $ part2 input

-- 883
-- 532
