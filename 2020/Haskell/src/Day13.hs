module Day13 where

import Data.List.Extra (foldl1', minimumOn, splitOn)
import Lib ( iterateFind )

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
part2 = fst . foldl1' firstCommon
  where
    firstCommon (x, stepx) (y, stepy) = (next, lcm stepx stepy)
      where
        next = iterateFind (\t -> (t + y) `mod` stepy == 0) (+ stepx) x

main :: IO ()
main = do
    (n, times) <- parseInput <$> readFile "../data/day13.in"
    print $ part1 n times
    print $ part2 times

-- 2165
-- 534035653563227
