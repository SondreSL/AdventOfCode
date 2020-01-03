import Data.Char
import Data.List.Extra
import qualified Data.Set as S
import qualified Data.Map.Strict as M

parse :: String -> [[Int]]
parse = map f . lines
  where
    f = map read . splitOn "," . filter num
    num n = isDigit n || n == '-' || n == ','

update :: [Int] -> [Int]
update [x, y, z, vx, vy, vz, ax, ay, az] = [x', y', z', vx', vy', vz', ax, ay, az]
  where 
    vx' = vx + ax
    vy' = vy + ay
    vz' = vz + az
    x' = x + vx'
    y' = y + vy'
    z' = z + vz'

manhatten :: [Int] -> Int
manhatten [x, y, z, vx, vy, vz, ax, ay, az] = abs x + abs y + abs z

collision :: [[Int]] -> [[Int]]
collision system = filter ((`S.notMember` colset) . take 3) system
  where 
    colset :: S.Set [Int]
    colset = M.keysSet
           . M.filter (>1)
           . M.fromListWith (+)
           . map (flip (,) 1 . take 3)
           $ system

part1 :: [[Int]] -> Int
part1 = fst
      . minimumOn snd 
      . zip [0..] 
      . map (\x -> sum x `div` length x) 
      . transpose 
      . take 1000
      . map (map manhatten) 
      . iterate (map update) 

part2 :: [[Int]] -> Int
part2 = length . (!! 1000) . iterate (map update . collision)

main = do
  input <- parse <$> readFile "data/20.in"
  print $ part1 input
  print $ part2 input

-- 150
-- 657
