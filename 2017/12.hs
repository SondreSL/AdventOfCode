import Data.Maybe
import Control.Arrow
import Data.List.Extra
import qualified Data.Set as S
import qualified Data.Map as M

type Id = Int
type Nodes = M.Map Id [Id]

parse' :: String -> Nodes
parse' = M.fromList 
       . map (((read . head) &&& (map read . drop 2)) 
             . splitOn " ") 
       . lines 
       . filter (','/=)

count :: Int -> Nodes -> S.Set Id
count start nodes = go S.empty $ M.findWithDefault [] start nodes
  where go seen [] = seen
        go seen xs = go (foldl (flip S.insert) seen xs) 
                     $ concatMap (\x -> M.findWithDefault [] x nodes)
                     $ filter (not . flip S.member seen) xs

part1 :: String -> Int
part1 = length . count 0 . parse'

part2 :: String -> Int
part2 = go 0 . parse'
  where
    go n keys
      | M.null keys = n
      | otherwise   = let new = fst . head $ M.toList keys
                       in go (n + 1) . foldl (flip M.delete) keys . S.toList $ count new keys 

main = do
  input <- readFile "data/12.in"
  print $ part1 input
  print $ part2 input

-- 115
-- 221
