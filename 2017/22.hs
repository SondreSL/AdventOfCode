import Data.List.Extra
import Data.Complex
import qualified Data.Map.Strict as M

type Grid = M.Map Pos Status
type Pos = (Int, Int)
type Dir = Complex Float

data Status = Clean
            | Infected 
            | Weak
            | Flagged
            deriving (Eq, Show)

data Burst = B { _grid :: Grid
               , _pos  :: Pos
               , _dir  :: Dir
               }
               deriving Show

parse :: String -> (Grid, Pos)
parse str = (grid, (len, len))
  where grid = M.fromList . filter ((==Infected) . snd) . zip co . map f . concat . lines $ str
        co = [(x,y) | y <- [0..(length $ lines str) - 1], x <- [0..(length (head $ lines str)) - 1]]
        len = (length $ lines str) `div` 2
        f '.' = Clean
        f '#' = Infected

turn :: Dir -> Status -> Dir
turn d Weak     = d
turn d Flagged  = d * (0:+1) * (0:+1)
turn d Clean    = d * (0:+(-1))
turn d Infected = d * (0:+1)

move :: Pos -> Dir -> Pos
move (x, y) (1:+1)       = (x,y-1) -- Up
move (x, y) (1:+(-1))    = (x-1,y) -- Left
move (x, y) ((-1):+1)    = (x+1,y) -- Right
move (x, y) ((-1):+(-1)) = (x, y+1) -- Down

evolve :: Status -> Status
evolve Clean = Weak
evolve Weak = Infected
evolve Infected = Flagged
evolve Flagged = Clean

weakEvolve :: Status -> Status
weakEvolve Clean = Infected
weakEvolve Infected = Clean
weakEvolve a = error $ show a

start :: (Grid, Pos) -> Burst
start (grid, pos) = B grid pos (1:+1)

burst :: (Pos -> Grid -> Grid) -> Burst -> Burst
burst nextState b@(B grid pos dir) = B grid' pos' dir'
  where
    dir' = turn dir $ M.findWithDefault Clean pos grid
    pos' = move pos dir'
    grid' = nextState pos grid

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs

part1 :: (Grid, Pos) -> Int
part1 = count status . take 10000 . iterate (burst nextState) . start
  where status (B grid pos _) = Clean == M.findWithDefault Clean pos grid
        nextState p g = M.insertWith (const weakEvolve) p Infected g

part2 :: (Grid, Pos) -> Int
part2 = count f . take 10000000 . iterate (burst nextState) . start
  where f (B grid pos dir) = Weak == M.findWithDefault Clean pos grid
        nextState p g = M.insertWith (const evolve) p Weak g

main = do
  input <- parse <$> readFile "data/22.in"

  print $ part1 input
  print $ part2 input

-- 5182
-- 2512008
