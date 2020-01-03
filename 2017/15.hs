import Data.Bits
import Data.Maybe
import Data.List.Extra

parse :: String -> [Int]
parse = map (read . last . words) . lines

judge :: Int -> Int -> Bool
judge a b = (a .&. 65535) == (b .&. 65535)

generate :: Int -> Int -> Int
generate seed val = val * seed `mod` 2147483647

part1 :: Int -> Int -> Int
part1 a b = length . filter id . take 40000000 $ zipWith judge as bs
  where
    as = iterate (generate 16807) a
    bs = iterate (generate 48271) b

part2 :: Int -> Int -> Int
part2 a b = length . filter id . take 5000000 $ zipWith judge as bs
  where
    as = filter ((==0) . (`mod` 4)) $ iterate (generate 16807) a
    bs = filter ((==0) . (`mod` 8)) $ iterate (generate 48271) b

main = do
  [a, b] <- parse <$> readFile "data/15.in"

  print $ part1 a b
  print $ part2 a b

