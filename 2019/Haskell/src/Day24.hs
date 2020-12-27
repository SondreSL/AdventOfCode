module Day24 where

-- import Control.Lens
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Lib
import Linear

parseInput = parseAsciiMap f
  where
    f '#' = Just True
    f '.' = Just False
    f n = error (show n)

step :: (Point -> [Point]) -> Map Point Bool -> Map Point Bool
step f mp = Map.mapWithKey next mp
  where
    next k v = case (v, length $ filter id $ mapMaybe (`Map.lookup` mp) $ f k) of
        (True, 1) -> True
        (False, 1) -> True
        (False, 2) -> True
        _ -> False

score :: Map Point Bool -> Int
score mp =
    sum
        . map fst
        . filter (snd . snd)
        . zip (iterate (* 2) 1)
        . sortOn fst
        . map (\(V2 x y, v) -> (V2 y x, v))
        $ Map.toList mp

part1 :: Map Point Bool -> Maybe Int
part1 = fmap score . firstRepeat . iterate (step neighbours4)

data Planet = Planet
    { _level :: Int -- Sits between (level - 1) and (level + 1)
    , _current :: Map Point Bool
    }
    deriving (Show, Eq, Ord)

mkPlanet :: Map Point Bool -> Map Int Planet
mkPlanet mp = Map.singleton 0 (Planet 0 mp')
  where
    mp' = Map.delete (V2 2 2) mp

extend :: Map Int Planet -> Map Int Planet
extend ps =
    let (mi, _) = Map.findMin ps
        (ma, _) = Map.findMax ps
        new = Map.delete (V2 2 2) $ Map.fromList $ map (,False) (sequenceA (V2 [0, 1, 2, 3, 4] [0, 1, 2, 3, 4]))
     in Map.insert (mi - 1) (Planet (mi - 1) new) $ Map.insert (ma + 1) (Planet (ma + 1) new) ps

stepPlanet :: Map Int Planet -> Map Int Planet
stepPlanet planets =
    let ext = extend planets
     in Map.map (step2 ext) ext

step2 :: Map Int Planet -> Planet -> Planet
step2 planets (Planet i mp) = Planet i (Map.mapWithKey next mp)
  where
    next k v = case (v, count id $ go k) of
        (True, 1) -> True
        (False, 1) -> True
        (False, 2) -> True
        _ -> False
    outside = maybe Map.empty _current (Map.lookup (i - 1) planets)
    inside = maybe Map.empty _current (Map.lookup (i + 1) planets)
    left = map (V2 0) [0 .. 4]
    right = map (V2 4) [0 .. 4]
    top = map (`V2` 0) [0 .. 4]
    bottom = map (`V2` 4) [0 .. 4]
    go p@(V2 2 1) = mapMaybe (`Map.lookup` inside) top ++ mapMaybe (`Map.lookup` mp) (neighbours4 p)
    go p@(V2 2 3) = mapMaybe (`Map.lookup` inside) bottom ++ mapMaybe (`Map.lookup` mp) (neighbours4 p)
    go p@(V2 1 2) = mapMaybe (`Map.lookup` inside) left ++ mapMaybe (`Map.lookup` mp) (neighbours4 p)
    go p@(V2 3 2) = mapMaybe (`Map.lookup` inside) right ++ mapMaybe (`Map.lookup` mp) (neighbours4 p)
    go pos
        | pos `elem` left && pos `elem` top =
            mapMaybe (`Map.lookup` outside) [V2 1 2, V2 2 1] ++ mapMaybe (`Map.lookup` mp) (neighbours4 pos)
        | pos `elem` left && pos `elem` bottom =
            mapMaybe (`Map.lookup` outside) [V2 1 2, V2 2 3] ++ mapMaybe (`Map.lookup` mp) (neighbours4 pos)
        | pos `elem` right && pos `elem` bottom =
            mapMaybe (`Map.lookup` outside) [V2 3 2, V2 2 3] ++ mapMaybe (`Map.lookup` mp) (neighbours4 pos)
        | pos `elem` right && pos `elem` top =
            mapMaybe (`Map.lookup` outside) [V2 3 2, V2 2 1] ++ mapMaybe (`Map.lookup` mp) (neighbours4 pos)
        | pos `elem` left = mapMaybe (`Map.lookup` outside) [V2 1 2] ++ mapMaybe (`Map.lookup` mp) (neighbours4 pos)
        | pos `elem` right = mapMaybe (`Map.lookup` outside) [V2 3 2] ++ mapMaybe (`Map.lookup` mp) (neighbours4 pos)
        | pos `elem` top = mapMaybe (`Map.lookup` outside) [V2 2 1] ++ mapMaybe (`Map.lookup` mp) (neighbours4 pos)
        | pos `elem` bottom = mapMaybe (`Map.lookup` outside) [V2 2 3] ++ mapMaybe (`Map.lookup` mp) (neighbours4 pos)
    go pos = mapMaybe (`Map.lookup` mp) (neighbours4 pos)

part2 :: Map Point Bool -> Int
part2 =
    sum
        . map (count id . Map.elems . _current)
        . Map.elems
        . (!! 200)
        . iterate stepPlanet
        . mkPlanet

main = do
    input <- parseInput <$> readFile "../data/test.in"
    print $ part1 input -- 2129920
    print $ part2 input --

    input <- parseInput <$> readFile "../data/input-2019-24.txt"
    print $ part1 input -- 20751345
    print $ part2 input

-- print $ part1 contents
-- print $ part2 contents
