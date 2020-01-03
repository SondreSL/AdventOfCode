import Data.Maybe
import Numeric
import Text.Printf
import Data.Char
import Data.Bits
import Data.Word
import Control.Lens
import Data.List.Extra hiding (disjoint)
import qualified Data.Set as S 
import qualified Data.Vector as V 

data Knot = K { _vec :: V.Vector Word8
              , _pos :: Word8
              , _skip :: Word8
              }

-- https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md#day-14
newtype DisjointSet = D { getD :: S.Set (S.Set (Int, Int)) }

instance Monoid DisjointSet where
  mempty   = D S.empty

instance Semigroup DisjointSet where
  xs <> ys = foldl' go ys (getD xs)
    where
      go (D zs) z = D (newGroup `S.insert` disjoints)
        where overlaps = S.filter (not . S.null . (`S.intersection` z)) zs
              disjoints = zs `S.difference` overlaps
              newGroup = S.unions $ z : S.toList overlaps

disjoint :: S.Set (Int, Int) -> DisjointSet
disjoint = D . S.singleton

tie :: Knot -> Word8 -> Knot
tie (K v p s) n = K v' p' s'
  where 
    ixs = fromIntegral . (+ p) <$> init [0..n]
    vals = map (v V.!) ixs
    v' = v V.// zip ixs (reverse vals)
    p' = p + s + n
    s' = s + 1

process :: [Word8] -> V.Vector Word8
process = _vec . foldl tie (K (V.fromList [0..255]) 0 0)

knot :: String -> [Word8]
knot = map (foldr1 xor) 
      . chunksOf 16
      . V.toList
      . process
      . concat . replicate 64
      . (++ [17, 31, 73, 47, 23])
      . map (fromIntegral . ord)

toBin :: String -> Int -> [Int]
toBin str n = map digitToInt . concatMap (printf "%08b") . knot . (++"-"++ show n) $ str

coords :: [[Int]] -> S.Set (Int, Int)
coords input = S.fromList . map fst . filter ((==1) . snd) . zip (go 127 127) $ concat input
  where 
    go xs ys = [(x, y) | y <- [0..ys], x <- [0..xs]]

mkGroups :: S.Set (Int, Int) -> DisjointSet
mkGroups xs = foldMap withNeighbours xs
  where
    withNeighbours x = disjoint . S.fromList $ x : neighbours x
    neighbours (x,y) = filter (\(a, b) -> S.member (a, b) xs && inBounds a && inBounds b) [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
    inBounds a = a >= 0 && a < 128

part1 :: [[Int]] -> Int
part1 = sum . map sum
 
part2 :: [[Int]] -> Int
part2 = S.size . getD . mkGroups . coords

main = do

  input <- init <$> readFile "data/14.in"
  ints <- pure $ map (toBin input) [0..127]
  print $ part1 ints
  print $ part2 ints

-- 8106
-- 1164
