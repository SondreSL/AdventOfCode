module Main where

import Lib
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day04HKD
import qualified Day04regex
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

run :: [Char] -> IO b -> IO b
run day f = putStrLn ("\n\tDay " ++ day) >> f

main :: IO ()
main = do
  run "01" Day01.main
  run "02" Day02.main
  run "03" Day03.main
  run "04" Day04.main
  -- run "05" Day05.main
  -- run "06" Day06.main
  -- run "07" Day07.main
  -- run "08" Day08.main
  -- run "09" Day09.main
  -- run "10" Day10.main
  -- run "11" Day11.main
  -- run "12" Day12.main
  -- run "13" Day13.main
  -- run "14" Day14.main
  -- run "15" Day15.main
  -- run "16" Day16.main
  -- run "17" Day17.main
  -- run "18" Day18.main
  -- run "19" Day19.main
  -- run "20" Day20.main
  -- run "21" Day21.main
  -- run "22" Day22.main
  -- run "23" Day23.main
  -- run "24" Day24.main
  -- run "25" Day25.main

