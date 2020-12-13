module Day13 where

import Data.List.Extra (foldl1', minimumOn, splitOn)
import Data.Tuple.Extra (first)

parseInput :: String -> (Integer, [(Integer, Integer)])
parseInput = (\x -> (read $ head x, concatMap f . zip [0 ..] . splitOn "," $ x !! 1)) . lines
  where
    f (_, "x") = []
    f (n, x) = [(n, read x)]

part1 :: Integer -> [(Integer, Integer)] -> Integer
part1 n xs = uncurry (*) . minimumOn snd $ do
    (_, x) <- xs
    pure (x, x - n `mod` x)

part2 :: [(Integer, Integer)] -> Integer
part2 =
    fst
        . foldl1' firstCommon
        . uncurry (zipWith firstHit)
        . first cycle
        . splitAt 1
  where
    firstHit (x, a) (y, b) = (start, lcm a b)
      where
        start = head $ filter ((== 0) . (`mod` a)) . dropWhile (< 0) . map (subtract (y - x)) $ iterate (+ b) b
    firstCommon (x, stepx) (y, stepy) = (next, lcm stepx stepy)
      where
        next = head $ filter ((== y) . (`mod` stepy)) $ iterate (+ stepx) x

main :: IO ()
main = do
    (n, times) <- parseInput <$> readFile "../data/day13.in"
    print $ part1 n times
    print $ part2 times

-- 2165
-- 534035653563227
