module Day02 where

import Data.List (
  find,
  group,
  sort,
 )
import Data.Monoid (Sum (..))

parse :: String -> [String]
parse = lines

pairTwoThree :: (Num a1, Num a2, Ord a3) => [a3] -> (Sum a1, Sum a2)
pairTwoThree str = (counts 2, counts 3)
 where
  els = group . sort $ str
  counts n =
    if any ((== n) . length) els
      then Sum 1
      else Sum 0

part2 :: Eq b => [[b]] -> Maybe [b]
part2 strs = map fst . filter (uncurry (==)) <$> find oneDiff [zip a b | a <- strs, b <- strs]
 where
  oneDiff = (== 1) . length . filter (uncurry (/=))

main :: IO ()
main = do
  input <- parse <$> readFile "../data/day02.in"
  print $ getSum . uncurry (*) . foldMap pairTwoThree $ input
  print $ part2 input

-- 8715
-- fvstwblgqkhpuixdrnevmaycd
