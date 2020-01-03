import Data.Maybe
import Data.List.Extra

parse :: String -> [(Int, Int)]
parse = map (\[a,b] -> (read a, read b)) . map (splitOn ": ") . lines

caught :: Int -> (Int, Int) -> Bool
caught delay (depth, range) = (depth + delay) `mod` (2 * (range - 1)) == 0

part1 :: String -> Int
part1 = sum . map (uncurry (*)) . filter (caught 0) . parse

part2 :: [(Int, Int)] -> Int
part2 input = fromJust $ find notCaught [0..]
  where 
    notCaught delay = all (not . caught delay) input

main = do
  input <- readFile "data/13.in"
  print $ part1 input
  print $ part2 $ parse input

-- 1876
-- 3964778
