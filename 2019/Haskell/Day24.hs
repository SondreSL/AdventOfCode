import Data.List
import Data.Maybe
import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S

type Pos = (Int, Int)
type Game = M.Map Pos (Int, Bool)

parse :: String -> Game
parse input = M.fromList . zip coords . zip (map (2^) [0..]) . map (=='#') . concat . lines $ input
  where ls = lines input 
        lx = (subtract 1) . length . head $ ls
        ly = (subtract 1) . length $ ls
        coords = concatMap (zip [0..lx] . repeat) $ [0..ly]

biodiv :: Game -> Int
biodiv game = sum . map (fst . snd) . filter (snd . snd) $ M.toList game

advance :: Game -> Game
advance game = M.mapWithKey evolve game
  where evolve :: Pos -> (Int, Bool) -> (Int, Bool)
        evolve pos (i, status) = (i, newStat status (livingNeighbours pos))
        livingNeighbours (x,y) = length $ filter (snd . (\x -> M.findWithDefault (0, False) x game)) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
        newStat st v = 
          case st of 
            True -> case v of 
                      1 -> True 
                      _ -> False 
            False -> case v of 
                       1 -> True 
                       2 -> True 
                       _ -> False

draw :: Game -> IO ()
draw game = mapM_ putStrLn $ map (map (repr . snd . fromJust . flip M.lookup game)) coords
  where coords = map (zip [0..4] . repeat) [0..4]
        repr True = '#'
        repr False = '.'
        res = map (map (repr . snd . snd))

solveA :: Game -> Int
solveA = findRep S.empty . iterate advance
  where findRep seen (x:xs) = let val = biodiv x
                               in if S.member val seen
                                     then val
                                     else findRep (S.insert val seen) xs

solveB :: undefined
solveB = undefined

main = do
  contents <- parse <$> readFile "data/input-2019-24.txt"
  print $ solveA contents

  -- test <- parse <$> readFile "test.txt"
  -- test3 <- parse <$> readFile "test2.txt"

