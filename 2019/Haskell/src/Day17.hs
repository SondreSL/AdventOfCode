module Day17 where

import Lib
import Intcode
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

findMap :: Intcode -> String
findMap ic = init . map chr $ untilHalt (Wait ic)

calibrate :: String -> Int
calibrate input = sum . map product . filter (all (`Set.member` points) . neighbours4) $ Set.toList points
  where
    points = Map.keysSet $ parseAsciiMap f input
    f '#' = Just ()
    f _ = Nothing

part1 :: Intcode -> Int
part1 = calibrate . findMap

part2 :: ProgramState -> Int
part2 = last . untilHalt

main :: IO ()
main = do
  inp <- parseIntcode <$> readFile "../data/input-2019-17.txt"
  print $ part1 $ Intcode 0 0 [] [] inp
  let cmd = "A,B,A,C,A,B,C,A,B,C\n"
      a = "R,8,R,10,R,10\n"
      b = "R,4,R,8,R,10,R,12\n"
      c = "R,12,R,4,L,12,L,12\n"
      n = "n\n"
  let inp' = Map.insert 0 2 inp
  print $ part2 $ Wait (Intcode 0 0 (cmd ++ a ++ b ++ c ++ n) [] inp')

-- 3608
-- 897426
