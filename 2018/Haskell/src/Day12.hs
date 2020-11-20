{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
module Day12 where
-- https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-12

import Data.Bifunctor (bimap)
import Data.Finite (Finite, finites)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Ctx = Set (Finite 5)
type Rules = Set Ctx


-- {{{ parsing
parseInput :: String -> (Set Int, Rules)
parseInput = bimap makeState makeRules . span (/= '\n')

makeState :: String -> Set Int
makeState =
  S.fromList
    . map fst
    . filter ((== '#') . snd)
    . zip [0 ..]
    . filter (`elem` ".#")

makeRules :: String -> Rules
makeRules =
  M.keysSet
    . M.filter id
    . M.fromList
    . map
      ( bimap parseLine head
          . splitAt 5
          . map (== '#')
          . filter (`elem` ".#")
      )
    . lines
 where
  parseLine = S.fromList . map fst . filter snd . zip finites

-- }}}

step :: Rules -> Set Int -> Set Int
step rules plants =
  S.fromDistinctAscList
    . filter go
    $ [S.findMin plants - 2 .. S.findMax plants + 2]
      where
        go p = neighbs `S.member` rules
          where
            neighbs = S.fromDistinctAscList
                    $ filter (\i -> (p - 2 + fromIntegral i) `S.member` plants) finites

-- | Shift all the points so the smallest value in the set is zero.
normalize :: Set Int -> (Int, Set Int)
normalize xs = (m, S.map (subtract m) xs)
  where m = S.findMin xs

findLoop :: Rules -> Set Int -> (Int, Int, Int) -- first loop, size of loop, shift
findLoop rules w0 = go (M.singleton w0 (0, 0)) 1 w0
  where
    go !seen !i !w = case M.lookup w'Norm seen of
                       Nothing -> go (M.insert w'Norm (mn, i) seen) (i + 1) w'
                       Just (seenMn, seenI) -> (seenI, i - seenI, mn - seenMn)
      where
        w' = step rules w
        (mn, w'Norm) = normalize w'

part1 :: Rules -> Set Int -> Int
part1 rules = sum . (!! 20) . iterate (step rules)

part2 :: Int -> Rules -> Set Int -> Int
part2 n rules w = sum . goN extra . S.map (+ (loopShift * looped)) . goN loopN $ w
  where
    (loopN, loopSize, loopShift) = findLoop rules w
    goN n w = (!! n) . iterate (step rules) $ w
    (looped, extra) = (n - loopN) `divMod` loopSize

main :: IO ()
main = do
  (start, rules) <- parseInput <$> readFile "../data/day12.in"
  print $ part1 rules start
  print $ part2 50000000000 rules start


-- 2823
-- 2900000001856
