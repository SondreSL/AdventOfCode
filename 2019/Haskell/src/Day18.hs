module Day18 where

import Linear
import Control.Lens
import Data.Char
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Lib
import qualified Data.Set as Set
import           Data.Set   ( Set )
import Control.Monad.State
import Data.Maybe
import qualified Data.PQueue.Prio.Min as PQ
import Data.PQueue.Prio.Min ( MinPQueue )

data Tile
    = Door Char
    | Key Char
    | Entrance
    | Floor
    deriving (Show, Eq, Ord)
makePrisms ''Tile

tileChar :: Tile -> Char
tileChar (Key c) = c
tileChar (Door c) = c
tileChar _ = error "No char here"

type Input = Map Point Tile
type Lab = Map Point [(Int, Point)]
type ExpState = (Input, Lab, Set Char, Set (Set Point, Set Char))

data Explorer = Explorer
    { _position :: [Point]
    , _keys :: Set Char
    }
    deriving (Show, Eq, Ord)
makeLenses ''Explorer

-- Tuple of length of current trip and explorer
type Position = (Int, Explorer)

changeOrder :: Explorer -> [Explorer]
changeOrder (Explorer ps ks) =
  case ps of
    [_] -> [Explorer ps ks]
    [a,b,c,d] -> [ Explorer [a,b,c,d] ks
                 , Explorer [b,c,d,a] ks
                 , Explorer [c,d,a,b] ks
                 , Explorer [d,a,b,c] ks
                 ]
    n -> error $ show n

bfs :: Explorer -> State ExpState [(Int, Explorer)]
bfs (Explorer (pos:ps) keys) = do
  (coords, interests, _, seen) <- get
  let int = Map.findWithDefault [] pos interests
      nbs = filter (\(_, Explorer p ks) -> (Set.fromList p, ks) `Set.notMember` seen)
            . mapMaybe (\(i, x) -> f (i, x, x `Map.lookup` coords))
            $ int
      f (_, _, Nothing) = Nothing
      f (i, p, Just Entrance) = Just (i, Explorer (p:ps) keys)
      f (i, p, Just Floor) = Just (i, Explorer (p:ps) keys)
      f (i, p, Just (Key c)) = Just (i, Explorer (p:ps) (Set.insert c keys)) -- Maybe keep going
      f (i, p, Just (Door c)) = guard (Set.member (toLower c) keys) >> Just (i, Explorer (p:ps) keys)
  pure nbs

explore :: MinPQueue Int Explorer -> State ExpState (Maybe (Int, Explorer))
explore queue = do
  (_, _, keys, seen) <- get
  ((st, exp), queue') <- maybe (error "No explorers left") pure (PQ.minViewWithKey queue)
  exps' <- traverse bfs $ changeOrder exp
  let exps = Set.fromList $ concat exps'
  if | _keys exp == keys -> pure $ Just (st, exp)
     | null exps -> explore queue'
     | otherwise -> do
       modify $ set _4 (Set.union seen (Set.map ((\(Explorer ps ks) -> (Set.fromList ps, ks)) . snd) exps))
       explore (foldr (\(i, e) acc -> PQ.insert (i + st) e acc) queue' exps)

run :: Input -> Maybe Int
run input = fst <$> evalState (explore (PQ.singleton 0 initial)) (input, interests, keys, Set.empty)
  where
    interests = mkGraph input
    initial = Explorer entrance Set.empty
    entrance = fmap fst . filter ((== Entrance) . snd) $ Map.toList input
    keys = Set.fromList . map tileChar . Map.elems $ Map.filter (has _Key) input

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/input-2019-18.txt"
    print $ run input                 -- 6316
    print $ run $ alteredLab input    -- 1648

-- Various functions for changing the graph
alteredLab :: Map Point Tile -> Map Point Tile
alteredLab input =
  let Just ent = fmap fst . find ((== Entrance) . snd) $ Map.toList input
      nbs = ent : neighbours ent
   in foldl (flip Map.delete) input nbs
       & Map.insert (ent + V2 1 1) Entrance
       & Map.insert (ent + V2 (-1) (-1)) Entrance
       & Map.insert (ent + V2 1 (-1)) Entrance
       & Map.insert (ent + V2 (-1) 1) Entrance

mkGraph :: Map Point Tile -> Map Point [(Int, Point)]
mkGraph mp = Map.mapWithKey findNext interest
  where
    interest = Map.filter (\x -> has _Key x || has _Door x || has _Entrance x) mp
    findNext :: Point -> Tile -> [(Int, Point)]
    findNext pos _ = go (Set.singleton pos) (foldr (PQ.insert 1) PQ.empty (neighbours4 pos))
    go seen queue
      | PQ.null queue = []
      | otherwise = let Just ((i,next), queue') = PQ.minViewWithKey queue
                        nbs = filter (`Set.notMember` seen) $ neighbours4 next
                        seen' = Set.union seen (Set.fromList nbs)
                        queue'' = foldr (PQ.insert (i + 1)) queue' nbs
                     in case Map.lookup next mp of
                          Just (Key _) -> (i, next) : go seen queue'
                          Just (Door _) -> (i, next) : go seen queue'
                          Just Entrance -> go seen' queue''
                          Just Floor -> go seen' queue''
                          Nothing -> go seen queue'

-- Parsing
parseInput :: String -> Map Point Tile
parseInput = parseAsciiMap f
  where
    f '.' = Just Floor
    f '@' = Just Entrance
    f '#' = Nothing
    f c
        | isLower c = Just (Key c)
        | isUpper c = Just (Door c)
        | otherwise = error $ show c
