module Day16 where

import Control.Monad.State (
    MonadState (get),
    MonadTrans (lift),
    evalStateT,
    modify,
 )
import Data.Bifunctor (Bifunctor (second))
import Data.Either (lefts, rights)
import Data.List.Extra (isPrefixOf, sortOn, splitOn, transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import Lib (commaNums)
import Text.ParserCombinators.Parsec

parseInput :: String -> (Rules, [Int], [[Int]])
parseInput input = (parsedRules, parsedTicket, parsedNearby)
  where
    [rules, ticket, nearby] = splitOn "\n\n" input
    parsedTicket = commaNums . (!! 1) . lines $ ticket
    parsedNearby = map commaNums . tail . lines $ nearby
    tuple :: Parser (Int, Int)
    tuple = (,) <$> (read <$> many1 digit) <*> (char '-' *> (read <$> many1 digit))
    parsedRules = either (error . show) Map.unions . traverse (parse parseRules "") . lines $ rules
    parseRules = do
        name <- manyTill anyChar (string ": ")
        a <- tuple
        string " or "
        b <- tuple
        pure $ Map.singleton name (a, b)

type Rule = String
type Rules = Map Rule ((Int, Int), (Int, Int))

validTicket :: Rules -> [Int] -> Either Int [Int]
validTicket (Map.elems -> rules) t = case filter (not . isValid rules) t of
    [] -> Right t
    xs -> Left $ sum xs
  where
    isValid ts x = any (inRange x) ts

inRange :: Int -> ((Int, Int), (Int, Int)) -> Bool
inRange x ((a, b), (c, d)) = (a <= x && x <= b) || (c <= x && x <= d)

createCandidates :: Rules -> [[Int]] -> Map Int (Set Rule)
createCandidates input = Map.unionsWith Set.intersection . map (isCand input)
  where
    isCand (Map.toList -> rules) xs =
        Map.fromList . zip [0 ..] . map Set.unions . transpose $ map (fitsRule xs) rules
    fitsRule xs (name, cs) =
        map
            ( \x ->
                if inRange x cs
                    then Set.singleton name
                    else Set.empty
            )
            xs

removeDetermined :: [(Int, Set Rule)] -> [(Int, Set Rule)]
removeDetermined [] = []
removeDetermined ((x, s) : xs) =
    case Set.toList s of
        [v] -> (x, s) : removeDetermined (sortOn (length . snd) (map (second $ Set.filter (/= v)) xs))
        _ -> (x, s) : xs

determineRules :: Map Int (Set Rule) -> [(Rule, Int)]
determineRules (Map.toList -> nearby) =
    head . flip evalStateT Set.empty $ do
        for (sortOn (Set.size . snd) nearby) $ \(i, cands) -> do
            seen <- get
            pick <- lift . Set.toList $ cands Set.\\ seen
            modify $ Set.insert pick
            pure (pick, i)

part1 :: Rules -> [[Int]] -> Int
part1 input xs = sum . lefts $ map (validTicket input) xs

part2 :: Rules -> [[Int]] -> [Int] -> Int
part2 rules valids ticket = product . map ((ticket !!) . snd) . filter (isPrefixOf "departure" . fst) $ ids
  where
    ids = determineRules (createCandidates rules valids)

main :: IO ()
main = do
    (rules, ticket, nearby) <- parseInput <$> readFile "../data/day16.in"
    print $ part1 rules nearby
    let validtickets = rights $ map (validTicket rules) nearby
    print $ part2 rules validtickets ticket

-- 22977
-- 998358379943
