import Data.Maybe
import Control.Monad
import Data.List.Extra
import qualified Data.Map.Strict as M

type Rules = (Grid -> Maybe Grid)
type Grid = [String]

parse :: String -> Rules
parse = rules' .  M.fromList . map (\(x:_:y:_) -> (slash x, slash y)) . map (splitOn " ") . lines
  where slash = splitOn "/" 
        rules' input = flip M.lookup input

findNext :: Rules -> Grid -> Grid
findNext rules str = head . mapMaybe rules $ rotations str 
  where rotations x = foldr (liftM2 ($)) [x] $ map (:[id]) [reverse, map reverse, transpose]

transform :: Rules -> Grid -> Grid
transform rules grid = combine . map (map $ findNext rules) $ zoom grid
  where l len = 2 + mod (length len) 2
        zoom grid = map (transpose . map (chunksOf (l grid))) $ chunksOf (l grid) grid
        combine = map concat . concatMap transpose

solve :: Rules -> Int -> Int
solve rules n = count (=='#') . concat . (!! n) . iterate (transform rules) $ [".#.","..#","###"]
  where count f xs = length $ filter f xs

part1 :: Rules -> Int
part1 r = solve r 5 

part2 :: Rules -> Int
part2 r = solve r 18 

main = do
  input <- parse <$> readFile "data/21.in"

  print $ part1 input
  print $ part2 input

-- 142
-- 1879071
