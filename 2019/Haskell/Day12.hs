import Data.List
import Data.Tuple.Extra

import Debug.Trace

type Pos = [Int]
type Vel = [Int]
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

gravity :: Int -> Int -> Int
gravity a b 
  | a < b = 1 
  | a > b = (-1) 
  | a == b = 0

period :: [Moon] -> Moon -> Int
period moons moon = (+1) . length . takeWhile (all (/= moon)) . iterate step . step $ moons

solveA :: [Moon] -> Int
solveA = sum . map energy . last . take 1000 . iterate step . step
  where energy moon = uncurry (*) . both (sum . map abs) $ moon

solveB :: [Moon] -> Int
solveB moons = foldr1 lcm . map (period moons) $ moons

main = do
  input <- pure $ initialize [[-7, -1, 6], [6, -9, -9], [-12, 2, -7], [4, -17, -12]]
  test <- pure $ initialize [[-1, 0, 2], [2, -10, -7], [4, -8, 8], [3, 5, -1]]
  test2 <- pure $ initialize [[-8, -10, 0], [5, 5,10], [2, -7, 3], [9, -8, -3]]

  print $ solveA input

  print $ solveB test

-- 11384
