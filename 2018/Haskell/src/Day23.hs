module Day23 where

import Data.SBV

import Data.List.Extra (maximumOn)
import Linear (V3 (..))
import Control.Monad ((<=<))
import Text.ParserCombinators.Parsec
    ( char,
      digit,
      newline,
      string,
      many1,
      sepBy,
      sepEndBy,
      (<|>),
      parse )

data Nanobot = N
  { _pos :: V3 Integer
  , _radius :: Integer
  }
  deriving (Show, Eq, Ord)

parseInput :: String -> [Nanobot]
parseInput =
  either (error . show) id
    . parse (sepEndBy parsePos newline) ""
 where
  parsePos = N <$> (string "pos=<" *> pos) <*> (string ">, r=" *> num)
  pos = (\[x, y, z] -> V3 x y z) <$> sepBy num (char ',')
  num =
    (negate . read <$> (char '-' *> many1 digit))
      <|> (read <$> many1 digit)

-- | Is the second nanobot within the range of the first one?
withinRange :: Nanobot -> Nanobot -> Bool
withinRange n0 n1 = sum (abs (_pos n1 - _pos n0)) <= _radius n0

part1 :: [Nanobot] -> Int
part1 ns = length . filter (withinRange bigBoy) $ ns
 where
  bigBoy = maximumOn _radius ns

distance :: (SInteger, SInteger, SInteger) -> Nanobot -> SInteger
distance (x, y, z) (N (V3 vx vy vz) r) =
  ite (abs (x - literal vx)
       + abs (y - literal vy)
       + abs (z - literal vz)
       .<= literal r)
       1
       0

solveConstraint :: [Nanobot] -> Goal
solveConstraint ns = do
  [x, y, z] <- sIntegers ["x", "y", "z"]
  let xs = map (distance (x,y,z)) ns
  maximize "Maximize counts" $ sum xs
  let c = abs x + abs y + abs z
  minimize "Dist from Origo" c

part2 :: [Nanobot] -> IO ()
part2 = print <=< optimize Lexicographic . solveConstraint

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day23.in"
  print $ part1 input
  part2 input
