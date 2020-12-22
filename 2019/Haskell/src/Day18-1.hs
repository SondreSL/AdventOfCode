module Day18_1 where

import Data.List
import Data.Char
import Data.Maybe
import Control.Lens
import Data.Bifunctor
import Control.Arrow ((&&&))
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
data Key = K { _name  :: Char
              , _dist  :: Int
              , _doors :: S.Set Char
              }
              deriving Show

data Explorer = E { _len       :: Int
                  , _keysFound :: S.Set Char
                  , _node      :: Key
                  }
                  deriving Show

nbs :: Labyrinth -> Seen -> Pos -> [Pos]
nbs lab seen (x,y) = filter notSeen . filter notWall $ [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
  where notWall p = not . wall $ M.findWithDefault Wall p lab
        notSeen x = not $ S.member x seen

bfs :: Labyrinth -> Pos -> M.Map Char Key
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
                       Key v  -> (drs, M.insert v (K v n drs) res)
                       Player -> (drs, res)
              queue' = P.deleteMin $ foldr (P.insert (n + 1)) queue (zip neighbours (repeat drs'))
           in go (foldr S.insert seen neighbours) queue' res'

startPos :: Labyrinth -> Pos
startPos lab = M.foldrWithKey (\k v acc -> if v == Player then k else acc) (0,0) lab

initialize :: Labyrinth -> M.Map Char (M.Map Char Key)
initialize lab = M.fromList . map (\(pos, Key v) -> (v, bfs lab pos)) . M.toList $ M.filter isKey lab

mkQueue :: Labyrinth -> P.MinPQueue Int Explorer
mkQueue lab = P.filter (\(E _ keysSeen (K _ _ drs)) -> drs `S.isSubsetOf` keysSeen) $ foldr (uncurry P.insert) P.empty $ map f ks
  where
    ks = M.toList $ bfs lab (startPos lab)
    f (ch, key) = (_dist key, E (_dist key) S.empty key)

findNextPath :: M.Map Char (M.Map Char Key) -> Char -> S.Set Char -> [Key]
findNextPath dists name seen =
  let f (K n d dr) = not (toUpper n `S.member` seen) && dr `S.isSubsetOf` seen
   in map snd . M.toList $ M.filter f $ M.findWithDefault M.empty name dists

path :: Labyrinth -> Int
path lab = go (mkQueue lab) (initialize lab) S.empty
  where
    go pq dists seen =
      let (pri, (E len keysSeen k@(K name dist drs))) = P.findMin pq -- Cannot fail
          keysSeen' = S.insert (toUpper name) keysSeen
          cands = findNextPath dists name keysSeen'
          nextExp newK@(K n d ds) = (len + d, E (len + d) keysSeen' newK)
          pq' = foldr (uncurry P.insert) (P.deleteMin pq) $ map nextExp cands
       in case S.member (name, keysSeen') seen of
            False -> case cands of
                       [] -> pri -- Finished
                       _ -> go pq' dists (S.insert (name, keysSeen') seen)
            True -> go (P.deleteMin pq) dists seen

solveA :: Labyrinth -> Int
solveA lab = path lab

solveB :: undefined
solveB = undefined

main = do
  lab <- parseLab <$> readFile "data/input-2019-18.txt"
  print $ solveA lab
  -- lab2 <- parseLab <$> readFile "test2.in"
  -- print $ solveA lab2

  -- lab3 <- parseLab <$> readFile "test3.in"
  -- print $ solveA lab3

  -- lab1 <- parseLab <$> readFile "test1.in"
  -- print $ solveA lab1
  -- lab <- parseLab <$> readFile "data/input-2019-18-2.txt"
  -- print $ solveB lab


-- 6316
