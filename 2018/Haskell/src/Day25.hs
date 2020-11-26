module Day25 where

import Lib ( commaNums
           , unionFind
           )
import Linear ( V4(..) )
import qualified Data.Set as Set
import           Data.Set   ( Set )

type Point = V4 Int

parseInput :: String -> [Point]
parseInput = map ((\[a,b,c,d] -> V4 a b c d) . commaNums) . lines

sameConstellation :: Point -> Point -> Bool
sameConstellation p q = sum (abs (p - q)) <= 3

inConstellation :: Point -> Set Point -> Bool
inConstellation p = not . null . Set.filter (sameConstellation p)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day25.in"
  print . length $ unionFind inConstellation input

-- 390
