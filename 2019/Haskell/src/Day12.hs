module Day12 where

import Data.List
import qualified Data.Set as S
import Data.Tuple.Extra

import Debug.Trace

type Pos = [Integer]
type Vel = [Integer]
type Moon = (Pos, Vel)

initialize :: [Pos] -> [Moon]
initialize = map (flip (,) [0,0,0])

step :: [Moon] -> [Moon]
step moons =
  let updateCoords (coords, vel) = (zipWith (+) coords vel, vel)
   in map updateCoords $ (map =<< newVelocity) moons

newVelocity :: [Moon] -> Moon -> Moon
newVelocity xs (we, sp) =
  let moons = map fst xs
      diff = map sum . transpose $ map (zipWith gravity we) moons
   in (we, zipWith (+) sp diff)

gravity :: Integer -> Integer -> Integer
gravity a b
  | a < b = 1
  | a > b = (-1)
  | a == b = 0

compVar :: Int -> Moon -> Moon -> Bool
compVar n self tar = (fst self !! n) == (fst tar !! n) && (snd self !! n) == (snd tar !! n)

findFirst :: S.Set Integer -> [Integer] -> Integer
findFirst set (x:xs)
  | S.member (div x 2) set = div x 2
  | otherwise = findFirst (S.insert x set) xs

period :: [Moon] -> Integer -> Integer
period moons n =
  let moon = moons !! (fromInteger n)
      matchingStates co = co moon . (!! (fromInteger n)) . snd
      ixs co = findFirst S.empty . map fst . filter (matchingStates co) . zip [1..] . iterate step . step $ moons
      x = ixs (compVar 0)
      y = ixs (compVar 1)
      z = ixs (compVar 2)
   in foldr lcm 1 [x,y,z]

solveA :: [Moon] -> Integer
solveA = sum . map energy . last . take 1000 . iterate step . step
  where energy moon = uncurry (*) . both (sum . map abs) $ moon

solveB :: [Moon] -> Integer
solveB moons =
  let a = map (period moons) $ [0..3]
   in foldr1 lcm a

main = do
  input <- pure $ initialize [[-7, -1, 6], [6, -9, -9], [-12, 2, -7], [4, -17, -12]]
  test  <- pure $ initialize [[-1, 0, 2], [2, -10, -7], [4, -8, 8], [3, 5, -1]]
  print $ solveB test
  print $ solveB input

-- 11384
-- 452582583272768

