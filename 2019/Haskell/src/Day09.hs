module Day09 where

import Data.Char
import Intcode

solveA :: Memory -> Int
solveA memory = head . untilHalt $ compute (Intcode 0 0 [chr 1] [] memory)

solveB :: Memory -> Int
solveB memory = head . untilHalt $ compute (Intcode 0 0 [chr 2] [] memory)

main :: IO ()
main = do
  contents <- parseIntcode <$> readFile "../data/input-2019-9.txt"
  print $ solveA contents
  print $ solveB contents

-- 3241900951
-- 83089

