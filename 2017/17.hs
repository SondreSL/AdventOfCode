import Data.Maybe
import Control.Lens
import Data.List.Extra

data State = S { _step   :: Int
               , _next   :: Int
               , _ix     :: Int
               , _buffer :: [Int]
               }
               deriving Show

simulate :: State -> State
simulate (S s next ix xs) = S s next' ix' xs'
  where 
    ix' = 1 + ((ix + s) `mod` next)
    xs' = (take ix' xs ++ [next] ++ drop ix' xs)
    next' = next + 1

part1 :: Int -> Int
part1 n = (!! 1) . dropWhile (/=2017) . _buffer . fromJust . find ((==2018) . _next) . iterate simulate $ S n 1 0 [0]

part2 :: Int -> Int
part2 n = last
        . elemIndices 1
        $ scanl f 0 [1..50000000]
  where
     f ix next = 1 + ((ix + n) `mod` next)


main = do
  input <- (read :: String -> Int) <$> readFile "data/17.in"
  print $ part1 3
  print $ part1 input
  print $ part2 input

-- 1306
-- 20430489
