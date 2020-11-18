{-# LANGUAGE ViewPatterns #-}
module Day10 where

import Lib ( display
           , findBounds
           )
import Linear (V2 (..))
import Text.ParserCombinators.Parsec (
  Parser,
  char,
  digit,
  many,
  many1,
  parse,
  space,
  string,
  (<|>),
 )

parseInput :: String -> [Coord]
parseInput str = case traverse (parse parseCoords "") $ lines str of
  Left err -> error $ show err
  Right v -> v

data Coord = Coord
  { pos :: V2 Int
  , vel :: V2 Int
  }
  deriving (Show)

--- {{{ Parsing
num :: Parser Int
num = neg <|> (read <$> many1 digit)
 where
  neg = negate . read <$> (char '-' *> many1 digit)

parseCoords :: Parser Coord
parseCoords = do
  string "position=<"
  many space
  x <- num
  char ','
  many space
  y <- num
  string "> velocity=<"
  many space
  xVel <- num
  char ','
  many space
  yVel <- num
  char '>'
  pure $ Coord (V2 x y) (V2 xVel yVel)

positions = map pos

---- }}}

step :: Coord -> Coord
step (Coord pos vel) = Coord (pos + vel) vel

applyVelocity :: [Coord] -> [Coord]
applyVelocity = map step

toTuple = map tup
  where tup (Coord (V2 x y) _) = (x, y)

findSmallestBound :: [[Coord]] -> [[Coord]]
findSmallestBound (cs : ds : rest) =
  if bigger (toTuple cs) (toTuple ds)
    then cs : findSmallestBound (ds : rest)
    else [cs]
 where
  bigger cs ds = boundSize (findBounds cs) > boundSize (findBounds ds)

boundSize :: (Int, Int, Int, Int) -> Int
boundSize (minx, miny, maxx, maxy) = (maxx - minx) * (maxy - miny)

part1 :: [[Coord]] -> String
part1 (last -> cs) = display pro (findBounds $ map toTuple cs) cs
  where
    pro (Coord (V2 x y) _) = ((y, x), True)
    toTuple (Coord (V2 x y) _) = (x, y)

part2 :: [[Coord]] -> Int
part2 = length

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day10.in"
  let bs = findSmallestBound $ tail $ iterate applyVelocity input
  putStr $ part1 bs
  print $ part2 bs

-- ZAEKAJGC
-- 10577
