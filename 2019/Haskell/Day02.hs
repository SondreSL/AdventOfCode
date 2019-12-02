import Data.List
import qualified Data.Vector as V
import Data.Bool

-- I preprossesed the file to remove commas and allow for simple splitting
-- using 'words'

strToInts :: String -> V.Vector Int
strToInts = V.fromList . map read . words

compute :: Int -> V.Vector Int -> Int
compute i vec
  | vec V.! i == 99 = vec V.! 0
  | otherwise =
    let code = bool (*) (+) (vec V.! i == 1)
        a = vec V.! (vec V.! (i + 1))
        b = vec V.! (vec V.! (i + 2))
        c = vec V.! (i + 3)
        new = code a b
     in compute (i + 4) (V.unsafeUpd vec [(c, new)])

solveA :: V.Vector Int -> Int
solveA = compute 0 . flip V.unsafeUpd [(1, 12), (2, 2)]

solveB :: V.Vector Int -> Int
solveB vec =
  let xs = [[i, j] | i <- [0..99], j <- [0..99]]
      f [a,b] = compute 0 (V.unsafeUpd vec [(1, a), (2, b)]) == 19690720
      Just (a:b:_) = find f xs
   in 100 * a + b

main = do
  contents <- strToInts <$> readFile "../data/input-2019-2.txt"
  print $ solveA contents
  print $ solveB contents
