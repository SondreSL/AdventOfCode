{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Day20 where

import Linear ( V2(..) )
import qualified Data.Set as Set
import           Data.Set   ( Set )
import qualified Text.Parsec as P
import Data.Maybe (mapMaybe)
import Control.Monad (guard)

data Tok = TokStart | TokEnd | TokAlt | TokRPar | TokLPar | DirTok Dir
         deriving (Show, Eq, Ord)

data Dir = N | E | S | W
  deriving (Show, Eq, Ord, Enum, Bounded)

type Point = V2 Int
data Edge = Edge Point Point
  deriving (Show, Eq, Ord)

-- | Parsing
parseInput :: String -> [Tok]
parseInput = mapMaybe $ \case
    '^' -> Just TokStart
    '$' -> Just TokEnd
    'N' -> Just $ DirTok N
    'E' -> Just $ DirTok E
    'S' -> Just $ DirTok S
    'W' -> Just $ DirTok W
    '|' -> Just TokAlt
    '(' -> Just TokLPar
    ')' -> Just TokRPar
    _ -> Nothing

mkEdge :: Point -> Point -> Edge
mkEdge x y
  | x <= y = Edge x y
  | otherwise = Edge y x

type Parser = P.Parsec [Tok] Point

-- | Logic
tok :: Tok -> Parser ()
tok t = P.try $ guard . (== t) =<< P.anyToken

anySteps :: Parser (Set Edge)
anySteps = fmap Set.unions . P.many $
           P.try normalStep P.<|> branchStep

normalStep :: Parser (Set Edge)
normalStep = do
  pos <- P.getState
  DirTok tok <- P.anyToken
  let pos' = pos + case tok of
                   N -> V2 0 (-1)
                   E -> V2 1 0
                   S -> V2 0 1
                   W -> V2 (-1) 0
  P.setState pos'
  Set.insert (mkEdge pos pos') <$> anySteps

branchStep :: Parser (Set Edge)
branchStep = P.between (tok TokLPar) (tok TokRPar) $ do
               pos <- P.getState
               fmap Set.unions . (`P.sepBy` tok TokAlt) $ do
                 P.setState pos
                 anySteps

buildSteps :: Parser (Set Edge)
buildSteps = P.between (tok TokStart) (tok TokEnd) anySteps

roomDistances :: Set Edge -> [Int]
roomDistances s0 = go 0 Set.empty (V2 0 0)
  where
    go !n !seen !pos = (n :) $
      concatMap (go (n + 1) (Set.insert pos seen)) ns
        where ns = filter ((`Set.member` s0) . mkEdge pos)
                   . filter (`Set.notMember` seen)
                   $ neighbours pos

neighbours :: Point -> [Point]
neighbours p = (p +) <$> [ V2 0 1, V2 1 0, V2 (-1) 0, V2 0 (-1)]

-- | Main
part1 :: Set Edge -> Int
part1 = maximum . roomDistances

part2 :: Set Edge -> Int
part2 = length . filter (>= 1000) . roomDistances

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day20.in"
  let Right edges = P.runParser buildSteps (V2 0 0) "" input
  print $ part1 edges
  print $ part2 edges

-- 3545
-- 7838
