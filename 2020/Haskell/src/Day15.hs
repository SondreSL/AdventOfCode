module Day15 where

import Data.IntMap (IntMap)
import qualified Data.IntMap as Map

play :: IntMap (Either (Int, Int) Int) -> Int -> Int -> [Int]
play seen turn lastSpoken =
    case Map.lookup lastSpoken seen of
        Just (Right _) -> 0 : play (Map.insertWith f 0 (Right turn) seen) (succ turn) 0
        Just (Left (prev, old)) ->
            let diff' = prev - old
             in diff' : play (Map.insertWith f diff' (Right turn) seen) (succ turn) diff'
  where
    f (Right new) (Right old) = Left (new, old)
    f (Right new) (Left (old, _)) = Left (new, old)

initialize :: [Int] -> [Int]
initialize xs = (xs ++) $ play (Map.fromList (zip xs (map pure [1 ..]))) (succ (length xs)) (last xs)

part1 :: [Int] -> Int
part1 = (!! 2019) . initialize

part2 :: [Int] -> Int
part2 = (!! 29999999) . initialize

main :: IO ()
main = do
    let input = [1, 20, 11, 6, 12, 0]
    print $ part1 input
    print $ part2 input

-- 1085
-- 10652
