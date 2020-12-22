module Day21 where

import Intcode
    ( Intcode(Intcode), ProgramState(Wait), untilHalt, parseIntcode )
import Data.Char ( chr )
import           Data.Map   ( Map )

part1 :: Map Int Int -> [Int]
part1 input = untilHalt (Wait ic)
  where
    ic = Intcode 0 0 rules [] input
    rules = unlines
      [ "NOT B J"  -- Hole at B
      , "NOT C T"  --   or hole at C
      , "OR T J"
      , "AND D J"  -- ground at D
      , "NOT A T"  -- hole at A
      , "OR T J"
      , "WALK"
      , "\n"
      ]

part2 :: Map Int Int -> [Int]
part2 input = untilHalt (Wait ic)
  where
    ic = Intcode 0 0 rules [] input
    rules = unlines
      [ "NOT B J"  -- Hole at B
      , "NOT C T"  --   or hole at C
      , "OR T J"
      , "AND D J"  -- Ground at D
      , "AND H J"  -- Ground at H
      , "NOT A T"  -- Hole at A
      , "OR T J"
      , "RUN"
      , "\n"
      ]

drawRobot :: [Int] -> IO ()
drawRobot xs = putStrLn (map chr (init xs))
               >> putStr "Score: "
               >> print (last xs)
               >> putStrLn "\n"

main :: IO ()
main = do
  input <- parseIntcode <$> readFile "../data/input-2019-21.txt"
  drawRobot $ part1 input
  drawRobot $ part2 input

-- 19348404
