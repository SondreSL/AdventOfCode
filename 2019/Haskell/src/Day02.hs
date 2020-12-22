import Data.List
import Data.Vector (Vector, (//), (!), fromList)
import Data.Bool

-- I preprossesed the file to remove commas and allow for simple splitting
-- using 'words'

strToInts :: String -> Vector Int
strToInts = fromList . map read . words

compute :: Int -> Vector Int -> Int
compute i vec
  | vec ! i == 99 = vec ! 0
  | otherwise =
    let code = bool (*) (+) (vec ! i == 1)
        a = vec ! (vec ! (i + 1))
        b = vec ! (vec ! (i + 2))
        c = vec ! (i + 3)
        new = code a b
     in compute (i + 4) (vec // [(c, new)])

solveA :: Vector Int -> Int
solveA = compute 0 . (// [(1, 12), (2, 2)])

solveB :: Vector Int -> Int
solveB vec =
  let xs = [[i, j] | i <- [0..99], j <- [0..99]]
      f [a,b] = compute 0 (vec // [(1, a), (2, b)]) == 19690720
      Just (a:b:_) = find f xs
   in 100 * a + b

main = do
  contents <- strToInts <$> readFile "data/input-2019-2.txt"
  print $ solveA contents
  print $ solveB contents
