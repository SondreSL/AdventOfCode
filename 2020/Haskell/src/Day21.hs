module Day21 where

import Control.Monad.State
  (MonadState (get), MonadTrans (lift), evalStateT, modify)
import Data.List.Extra (intercalate, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import Lib (count)
import Text.ParserCombinators.Parsec
  (between, char, letter, many1, parse, sepBy, sepEndBy, space, string)

parseInput :: String -> [([String], [String])]
parseInput = either (error . show) id . traverse (parse p "") . lines
  where
    p =
        (,)
            <$> sepEndBy (many1 letter) space
            <*> between
                (char '(')
                (char ')')
                (string "contains " *> sepBy (many1 letter) (string ", "))

candidates :: [([String], [String])] -> Map String (Set String)
candidates xs = Map.unionsWith Set.intersection $ concatMap f xs
  where
    f (a, b) = map (\x -> Map.singleton x (Set.fromList a)) b

part1 :: [([String], [String])] -> Int
part1 input = count (`Set.notMember` cs) (concatMap fst input)
  where
    cs = Set.unions . Map.elems $ candidates input

part2 :: [([String], [String])] -> Maybe String
part2 input = intercalate "," . map snd . sortOn fst <$> res
  where
    cs = Map.toList $ candidates input
    res = listToMaybe $
        flip evalStateT Set.empty $
            for cs $ \(a, ing) -> do
                seen <- get
                pick <- lift $ Set.toList $ ing Set.\\ seen
                modify $ Set.insert pick
                pure (a, pick)

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day21.in"
    print $ part1 input
    print $ part2 input

-- 2798
-- gbt,rpj,vdxb,dtb,bqmhk,vqzbq,zqjm,nhjrzzj
