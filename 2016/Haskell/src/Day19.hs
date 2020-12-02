module Day19 where

import Data.Sequence ( Seq(..) )
import qualified Data.Sequence as S

n :: Int
n = 3012210

type Elves = Seq Int

steal :: (Elves -> Int) -> Elves -> Elves
steal _ es@(_ :<| Empty) = es
steal f es =
  let (e :<| rest) = S.deleteAt (f es) es
   in rest S.|> e

run :: (Elves -> Int) -> Elves -> Int
run f = flip S.index 0 . head . dropWhile ((>1) . length) . iterate (steal f)

part1 :: Elves -> Int
part1 = run (const 1)

part2 :: Elves -> Int
part2 = run ((`div` 2) . length)

main :: IO ()
main = do
  print $ part1 (S.fromList [1 .. n])
  print $ part2 (S.fromList [1 .. n])

-- 1830117
-- 1417887
