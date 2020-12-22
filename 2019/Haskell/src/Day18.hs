module Day18 where

import Data.List
import Data.Char
import Data.Maybe
import Control.Lens
import Data.Bifunctor
import qualified Data.Set as S
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Map.Strict as M

import Debug.Trace

-- Definitions
data Object = Key Char
            | Door Char
            | Path
            | Player
            | Wall
            deriving (Eq, Show)

type Pos = (Int, Int)
type Labyrinth = M.Map Pos Object
type Dists = M.Map Pos Int
type Cache = M.Map (Pos, Pos) Int
type Seen = S.Set Pos

wall :: Object -> Bool
wall Wall = True
wall _ = False

isKey :: Object -> Bool
isKey (Key _) = True
isKey _ = False

isKeyPlayer :: Object -> Bool
isKeyPlayer (Key _) = True
isKeyPlayer Player = True
isKeyPlayer _ = False

value :: Object -> Char
value (Key v) = v
value (Door v) = v

-- Parsing and drawing
parseLab :: String -> Labyrinth
parseLab input =
  let ls = lines input
      maxY = length ls
      maxX = (subtract 1) . length . head $ ls
      coords = concatMap (zip [0..maxX] . repeat) [0..maxY]
      object o
        | o == '.' = Path
        | o == '#' = Wall
        | o == '@' = Player
        | isAlpha o && isLower o = Key o
        | isAlpha o && isUpper o = Door o
   in M.fromList . filter ((/=Wall) . snd) . map (second object) . zip coords $ concat ls

-- Logic
data Point = K { _name  :: Char
             , _pos   :: Pos
             , _dist  :: Int
             , _doors :: S.Set Char
             }
               | P Pos
             deriving Show

data Explorer = E { _len       :: Int
                  , _keysFound :: S.Set Char
                  , _node      :: Point
                  }
                  deriving Show

nbs :: Labyrinth -> Seen -> Pos -> [Pos]
nbs lab seen (x,y) = filter notSeen . filter notWall $ [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
  where notWall p = not . wall $ M.findWithDefault Wall p lab
        notSeen x = not $ S.member x seen

bfs :: Labyrinth -> Pos -> M.Map Char Point
bfs lab initPos = go initialSeen initialQueue M.empty
  where
    initialQueue = foldr (P.insert 1) P.empty $ zip (nbs lab (S.singleton initPos) initPos) (repeat S.empty)
    initialSeen = foldr S.insert (S.singleton initPos) $ nbs lab S.empty initPos
    go seen queue res =
      case P.getMin queue of
        Nothing -> res
        Just (n, (pos, drs)) ->
          let neighbours = nbs lab seen pos
              val = M.findWithDefault Path pos lab
              (drs', res') = case val of
                       Path   -> (drs, res)
                       Door v -> (S.insert v drs, res)
                       Key v  -> (drs, M.insert v (K v pos n drs) res)
                       Player -> (drs, res)
              queue' = P.deleteMin $ foldr (P.insert (n + 1)) queue (zip neighbours (repeat drs'))
           in go (foldr S.insert seen neighbours) queue' res'

startPos :: Labyrinth -> Pos
startPos = M.foldrWithKey (\k v acc -> if v == Player then k else acc) (0,0)

initialize :: Labyrinth -> M.Map Char (M.Map Char Point)
initialize lab = M.fromList . map (\(pos, Key v) -> (v, bfs lab pos)) . M.toList $ M.filter isKey lab

-- | Make the initial queue for a labyrinth
mkQueue :: Labyrinth -> P.MinPQueue Int Explorer
mkQueue lab = P.filter isSubsetOf . foldr (uncurry P.insert) P.empty $ map toExplorer initialDists
  where
    initialDists = M.toList $ bfs lab (startPos lab)
    toExplorer (ch, key) = (_dist key, E (_dist key) S.empty key)
    isSubsetOf (E _ keysSeen (K _ _ _ drs)) = drs `S.isSubsetOf` keysSeen

findNextPath :: M.Map Char (M.Map Char Point) -> Char -> S.Set Char -> [Point]
findNextPath dists name seen = map snd . M.toList . M.filter f $ M.findWithDefault M.empty name dists
  where
    f (K n pos d dr) = not (toUpper n `S.member` seen) && dr `S.isSubsetOf` seen

path :: Labyrinth -> Int
path lab = go (mkQueue lab) (initialize lab) S.empty
  where
    go pq dists seen =
      let (pri, (E len keysSeen k@(K name pos dist drs))) = P.findMin pq -- Cannot fail
          keysSeen' = S.insert (toUpper name) keysSeen
          cands = findNextPath dists name keysSeen'
          nextExp newK@(K n p d ds) = (len + d, E (len + d) keysSeen' newK)
          pq' = foldr (uncurry P.insert) (P.deleteMin pq) $ map nextExp cands
       in case S.member (name, keysSeen') seen of
            False -> case cands of
                       [] -> pri -- Finished
                       _ -> go pq' dists (S.insert (name, keysSeen') seen)
            True -> go (P.deleteMin pq) dists seen

-- For multiple labyrinths
startPositions :: Labyrinth -> [Pos]
startPositions = M.foldrWithKey (\k v acc -> if v == Player then k:acc else acc) []

bfsPos :: Labyrinth -> Pos -> M.Map Pos Point
bfsPos lab initPos = go initialSeen initialQueue M.empty
  where
    initialQueue = foldr (P.insert 1) P.empty $ zip (nbs lab (S.singleton initPos) initPos) (repeat S.empty)
    initialSeen = foldr S.insert (S.singleton initPos) $ nbs lab S.empty initPos
    go seen queue res =
      case P.getMin queue of
        Nothing -> res
        Just (n, (pos, drs)) ->
          let neighbours = nbs lab seen pos
              val = M.findWithDefault Path pos lab
              (drs', res') = case val of
                       Path   -> (drs, res)
                       Door v -> (S.insert v drs, res)
                       Key v  -> (drs, M.insert pos (K v pos n drs) res)
                       Player -> (drs, M.insert pos (P pos) res)
              queue' = P.deleteMin $ foldr (P.insert (n + 1)) queue (zip neighbours (repeat drs'))
           in go (foldr S.insert seen neighbours) queue' res'

-- mkQueue4 :: Labyrinth -> P.MinPQueue Int Explorer
-- mkQueue4 lab = P.fromList
--   where
--     intitial = startPositions lab

initializePos :: Labyrinth -> M.Map Pos (M.Map Pos Point)
initializePos lab = M.fromList . map (\(pos, v) -> (pos, bfsPos lab pos)) . M.toList $ M.filter isKeyPlayer lab

path4 :: Labyrinth -> Int
path4 lab = undefined

solveA :: Labyrinth -> Int
solveA = path

solveB :: undefined
solveB = undefined

main = do
  lab <- parseLab <$> readFile "data/input-2019-18.txt"
  -- print $ solveA lab


  lab2 <- parseLab <$> readFile "test2.in"
  print $ startPositions lab2
  mapM_ print $ initializePos lab2
  -- mapM_ print $ map (bfsPos lab2) (startPositions lab2)

  -- lab3 <- parseLab <$> readFile "test3.in"
  -- print $ solveA lab3

  -- lab1 <- parseLab <$> readFile "test1.in"
  -- print $ solveA lab1
  -- lab <- parseLab <$> readFile "data/input-2019-18-2.txt"
  -- print $ solveB lab


-- 6316
