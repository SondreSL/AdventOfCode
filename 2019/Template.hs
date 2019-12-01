import Data.List

strToInts :: String -> [Int]
strToInts = map read . lines

solve :: String -> String
solve = undefined

main = do
  contents <- readFile "data/"
  print contents
