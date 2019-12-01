-- Advent of Code 2019 Day 1
-- Sondre Lunde

strToInts :: String -> [Int]
strToInts = map read . lines

calcFuel :: Int -> Int
calcFuel = subtract 2 . flip div 3

solve :: [Int] -> Int
solve = sum . map calcFuel

solve2 :: [Int] -> Int
solve2 = sum . map calcFuelFuel
  where calcFuelFuel = sum . takeWhile (> 0) . iterate calcFuel . calcFuel

main :: IO ()
main = do
  contents <- strToInts <$> readFile "data/input-2019-1.txt"
  print $ solve contents
  print $ solve2 contents

-- Part 1: 3497998
-- Part 2: 5244112
