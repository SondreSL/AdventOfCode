import Data.Maybe
import Data.List.Extra
import qualified Data.Map.Strict as M

type Node = Int
type Graph = M.Map Node [Node]

type Edge = (Int, Int)

parseEdge :: String -> [Edge]
parseEdge = map (toTup . map read . splitOn "/")
          . lines
  where 
    toTup [x,y] = (x,y)

parse :: String -> Graph
parse = (nub <$>)
      . foldl f M.empty 
      . map (toTup . map read . splitOn "/")
      . lines
  where
    f acc (k, v) = M.insertWith (++) v [k] $ M.insertWith (++) k [v] acc
    toTup [x,y] = (x,y)

-- | Find all paths through the graph, starting at Node n
move :: Graph -> (Maybe Node, Node) -> [[Node]]
move graph (prev, n) = addSelf . concatMap (move graph') . zip (repeat (Just n)) $ M.findWithDefault [] n graph'
  where
    addSelf xs = if null xs then [[n]] else map (n:) xs
    edges = M.findWithDefault [] n graph
    graph' = if isNothing prev
                then graph 
                else M.adjust (delete n) (fromJust prev) $ M.adjust (delete (fromJust prev)) n graph

edgesum :: [Int] -> Int
edgesum [] = 0
edgesum (x:xs) = (2 * sum (init xs)) + last xs

part1 :: Graph -> Int
part1 = maximum . map edgesum . flip move (Nothing, 0)

part2 :: Graph -> Int
part2 g = maximum . map edgesum . filter ((==maxlen) . length) $ bridges
  where
    bridges = move g (Nothing, 0)
    maxlen = maximum $ map length bridges

main = do
  input <- parse <$> readFile "data/24.in"

  print $ part1 input
  print $ part2 input

-- 1511
-- 1471
