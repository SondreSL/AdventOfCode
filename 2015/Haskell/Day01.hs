module Main where

left :: Int -> Char -> Int
left n '(' = n + 1
left n ')' = n - 1

part1 :: String -> Int
part1 = foldl left 0

part2 :: String -> Int
part2 = length . takeWhile (0 <=) . scanl left 0

main :: IO ()
main = do
  input <- init <$> readFile "../data/01.in"
  print $ part1 input
  print $ part2 input
