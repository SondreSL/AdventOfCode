module Day07knot where

import Text.ParserCombinators.Parsec
import Control.Monad (replicateM_)
import qualified Data.Map as Map
import           Data.Map   ( Map )
import qualified Data.Set as Set
import Relude ((<&>))
import Data.Monoid

type Graph k v = Map k (Map k v)

parseInput :: String -> Graph String Int
parseInput = Map.fromList . either (error . show) id . traverse (parse p "") . lines
  where
    word = many1 letter
    content =
     flip (,) <$> (read <$> many1 digit) <* space
              <*> ((++) <$> many1 letter <* space
                        <*> many1 letter <* space <* word)
    p = do
      fabric <- word <* space
      col <- word <* space
      replicateM_ 2 (word <* space)
      rules <- sepBy content (string ", ")
      pure (fabric <> col, Map.fromList rules)

flippedMap :: Ord k => Graph k v -> Graph k v
flippedMap input = Map.fromListWith Map.union $ do
  (k,vs) <- Map.toList input
  (n,cs) <- Map.toList vs
  pure (n, Map.singleton k cs)

-- Specific versions
descendantMap :: Graph String Int -> Int
descendantMap (flippedMap -> input) = length $ descendants Map.! "shinygold"
  where
    descendants = input <&>
      Map.foldMapWithKey (\k _ -> Set.insert k (Map.findWithDefault Set.empty k descendants))

usages :: Graph String Int -> Int
usages input = getSum $ folded Map.! "shinygold"
  where
    folded = input <&>
      Map.foldMapWithKey (\k v -> Sum v * (Map.findWithDefault (Sum 0) k folded + 1))

-- Generalized version
foldMapGraph :: (Ord v, Monoid m) => (v -> m) -> (e -> m -> m) -> Graph v e -> Map v m
foldMapGraph f g input = folded
  where
    folded = input <&>
      Map.foldMapWithKey (\k v -> f k <> foldMap (g v) (Map.lookup k folded))

foldMapGraph' :: (Ord v, Monoid m) => (v -> m) -> (e -> m -> m) -> Graph v e -> Map v m
foldMapGraph' f g input = folded
  where
    folded = input <&>
      Map.foldMapWithKey (\k v -> f k <> g v (Map.findWithDefault mempty k folded))

part1 :: Graph String Int -> Int
part1 = length . (Map.! "shinygold") . foldMapGraph Set.singleton (const id) . flippedMap

part2 :: Graph String Int -> Sum Int
part2 = (Map.! "shinygold") . foldMapGraph (const $ Sum 0) (\o n -> Sum o * (n + 1))


main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day07.in"
  print $ descendantMap input
  print $ usages input
  print $ part1 input
  print $ part2 input

-- 226
-- 9569
