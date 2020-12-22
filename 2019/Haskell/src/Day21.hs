module Day21 where

import Lib
import Intcode
import Control.Lens
import Data.List.Extra
import Data.Char

part1 input = untilHalt (Wait ic)
  where
    ic = Intcode 0 0 rules [] input
    rules = unlines
      [ "NOT A J"
      , "NOT D T"
      , "NOT T T"
      , "OR T J"
      , "WALK"
      , "\n"
      ]

part2 input = undefined

jumpIntoHole :: String
jumpIntoHole = "NOT D J\nWALK\n\n"

drawRobot :: [Int] -> IO ()
drawRobot xs = putStrLn (map chr (init xs)) >> print (last xs)

main :: IO ()
main = do
  input <- parseIntcode <$> readFile "../data/input-2019-21.txt"
  drawRobot $ part1 input
  -- print $ part1 input
  -- print $ part2 input

-- If A is hole
--   -> Jump
-- Else
--   If D is not not hole
--     Jmp

