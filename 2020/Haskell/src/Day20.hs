module Day20 where

import Control.Monad.State
    ( guard, MonadState(put, get), MonadTrans(lift), StateT, evalStateT )
import Data.Bifunctor ( Bifunctor(first) )
import Data.List.Extra ( transpose, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Lib
    ( neighbours4, select, tuple, Point )
import Linear (V2 (V2), perp)
import Text.ParserCombinators.Parsec
  (digit, many1, oneOf, parse, string)

type Tile = (Int, [[Bool]])

-- Parsing
parseInput :: [Char] -> [Tile]
parseInput = either (error . show) id . traverse (parse parseTile "") . splitOn "\n\n"
  where
    f '.' = False
    f '#' = True
    parseTile = do
        idN <- string "Tile " *> many1 digit <* string ":\n"
        tiles <- map (map f) . lines <$> many1 (oneOf ".#\n")
        pure (read idN, tiles)

-- For part 1
findMatches :: [Tile] -> Maybe (Map Point Tile)
findMatches xs = listToMaybe $ evalStateT (f size (tail xs)) (Map.singleton (V2 0 0) (head xs))
  where
    size = round $ sqrt (fromIntegral $ length xs)
    f m ys = do
        (t, rest) <- lift $ select ys
        match m t
        if null rest
            then get >>= pure
            else f m rest

match :: Int -> Tile -> StateT (Map Point Tile) [] ()
match _ (i, t) = do
    ms <- get
    p <- lift $ possiblePositions ms -- Possible positions in the map
    let ns = mapMaybe (\x -> (x,) <$> Map.lookup x ms) (neighbours4 p) -- Neighbours for current possible position
    t' <- lift $ permute t
    guard $ all (doesFit p (i, t')) ns
    put $ Map.insert p (i, t') ms
      where
        permute t = take 4 (iterate rot90 t) ++ take 4 (iterate rot90 revved)
          where
            rot90 = map reverse . transpose
            revved = map reverse t

doesFit :: Point -> Tile -> (Point, Tile) -> Bool
doesFit p (_, t) (neigh, (_, n)) =
    case p - neigh of
        -- neighbour is above
        V2 0 (-1) -> head t == last n
        -- neighbour is below
        V2 0 1 -> last t == head n
        -- neighbour is right
        V2 (-1) 0 -> map head t == map last n
        -- neighbour is left
        V2 1 0 -> map last t == map head n

possiblePositions :: Map Point Tile -> [Point]
possiblePositions xs = filter (`Map.notMember` xs) . concat . Set.toList . Set.map neighbours4 . Map.keysSet $ xs

-- For part 2
toCoords :: Map Point Tile -> Set Point
toCoords = Set.unions . map Set.fromList . Map.elems . Map.mapWithKey tileCoords
  where
    tileCoords p (_, bs) = do
      (y, rows) <- zip [7,6..] . tail . init $ bs
      (x, cs) <- zip [7,6..] . tail . init $ rows
      guard cs
      pure $ V2 x y + (p * V2 8 8)

locateMonster :: Set Point -> Int
locateMonster xs = Set.size xs - Set.size points
  where
    ps = rotateCoords xs
    points = Set.unions $ concatMap removeSeamonster ps
    removeSeamonster ps = mapMaybe (isMonster ps) monsters
    offsets = map tuple $ sequence [[0 .. 8 * 12], [0 .. 8 * 12]]
    monsters = map (\(x, y) -> Set.map (+ V2 x y) seamonster) offsets
    isMonster coords xs =
        if Set.isSubsetOf xs coords
            then Just xs
            else Nothing
    rotateCoords xs = take 4 (iterate rot90 xs) ++ take 4 (iterate rot90 reversed)
      where
        rot90 = Set.map (subtract (V2 (-8*12) 0) . perp)
        reversed = Set.map (\(V2 x y) -> V2 (8*12 - x) y) xs

normalize :: (Num k, Ord k) => Map k a -> Map k a
normalize xs = Map.fromList . map (first (subtract minKey)) $ Map.toList xs
  where
    minKey = minimum $ Map.keys xs

part1 :: Map Point Tile -> Maybe Int
part1 = cornerProd . normalize
  where
    cornerProd xs = product . map fst <$> traverse (`Map.lookup` xs) [V2 0 0, V2 0 11, V2 11 11, V2 11 0]

part2 :: Map Point Tile -> Int
part2 = locateMonster . toCoords . normalize

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day20.in"
    let p1 = findMatches input
    print $ part1 =<< p1
    print $ part2 <$> p1

-- 51214443014783
-- 2065

seamonster :: Set Point
seamonster =
    Set.fromList $
        map
            (uncurry V2)
            [ (0, 1)
            , (1, 0)
            , (4, 0)
            , (5, 1)
            , (6, 1)
            , (7, 0)
            , (10, 0)
            , (11, 1)
            , (12, 1)
            , (13, 0)
            , (16, 0)
            , (17, 1)
            , (18, 1)
            , (19, 1)
            , (18, 2)
            ]

