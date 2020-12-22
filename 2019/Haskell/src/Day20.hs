import Data.Maybe
import Data.List.Extra
import Control.Arrow ((&&&))
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Prio.Min as P

data Point = Wall
           | Path
           | Letter Char
           | Portal String
           | Space
           deriving (Eq, Show)

type Pos = (Int, Int)
type Maze = M.Map Pos Point

-- Parse and initialize
spaceWall :: Point -> Bool
spaceWall x = x == Wall || x == Space

letter :: Point -> Bool
letter (Letter c) = True
letter _          = False

coords :: Int -> Int -> [(Int, Int)]
coords x y = [(x, y) | y <- [0..y], x <- [0..x]]

neighbours :: Pos -> [Pos]
neighbours (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

withDef :: Maze -> Pos -> Point
withDef m p = M.findWithDefault Path p m

parseMaze :: String -> Maze
parseMaze str = M.filter (not . spaceWall) . M.map toPoint . M.fromList . zip (coords x y) . filter (/='\n') $ str
  where x = (length . head $ lines str) - 1
        y = (length $ lines str) - 1
        toPoint x = case x of
                      ' ' -> Space
                      '#' -> Wall
                      '.' -> Path
                      _   -> Letter x

findPortals :: Maze -> Maze
findPortals maze = M.filter (not . letter) $ M.mapWithKey portalName portals `M.union` maze
  where
    portals = M.filterWithKey nextToLetter $ M.filter (==Path) maze
    nextToLetter pos _ = any (letter . withDef maze) $ neighbours pos
    portalName pos _ = Portal . sort . map (\(Letter v) -> v) $ getLetter pos
    getLetter pos = map (withDef maze) . fromJust . find (letter . withDef maze . head) $ allNbs pos
    allNbs (x,y) = [[(x+1,y),(x+2,y)],[(x-2,y),(x-1,y)],[(x,y+2),(x,y+1)],[(x,y-1),(x,y-2)]]

-- Pathfinding
initialize :: String -> Maze
initialize = findPortals . parseMaze

portalNeighbours :: Maze -> Int -> Pos -> [(Int, Pos)]
portalNeighbours maze level pos = matchingPortal maze level pos ++ zip (repeat level) (neighbours pos)

matchingPortal :: Maze -> Int -> Pos -> [(Int, Pos)]
matchingPortal maze level pos@(x,y) = 
  case withDef maze pos of 
    Portal s -> let next = M.foldrWithKey (\k v acc -> if v == Portal s && k /= pos then k else acc) (0,0) maze
                    (xs, ys) = (map fst &&& map snd) $ M.keys maze
                    minx = [minimum xs, maximum xs] 
                    miny = [minimum ys, maximum ys]
                    level' = if x `elem` minx || y `elem` miny
                               then level - 1
                               else level + 1
                 in [(level', next)]
    other    -> []

bfs :: (Int -> Bool) -> Maze -> Int
bfs f maze = go (S.singleton (0, start)) (P.singleton 0 (0, start))
  where
    end = M.foldrWithKey (\k v acc -> if v == Portal "ZZ" then k else acc) (0,0) maze
    start = M.foldrWithKey (\k v acc -> if v == Portal "AA" then k else acc) (0,0) maze
    go seen queue = 
      let (pri, (level, currentPos)) = P.findMin queue
          new = filter (\x -> not (x `S.member` seen) && (snd x `M.member` maze) && fst x >= 0) $ portalNeighbours maze level currentPos
          queue' = P.deleteMin . foldr (P.insert (pri + 1)) queue $ new
          seen' = foldr S.insert seen new
       in case (level, withDef maze currentPos) of
            (n, Portal "ZZ") -> if f n
                                   then pri -- Finished
                                   else go seen' queue'
            other -> go seen' queue'

solveA :: String -> Int
solveA = bfs (const True) . initialize

solveB :: String -> Int
solveB = bfs (==0) . initialize

main = do
  input <- readFile "data/input-2019-20.txt"
  print $ solveA input
  print $ solveB input

-- 618
-- 7152
