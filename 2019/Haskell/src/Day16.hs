import Data.List
import Data.Char

type State = [Int]
type Pattern = [Int]

strToInts :: String -> State
strToInts = map (read . pure)

pattern :: [Int] -> [Pattern]
pattern base = map (drop 1 . inner) [1..]
  where inner n = cycle $ concatMap (replicate n) base

phase :: State -> [Pattern] -> State
phase base patterns = take (length base) $ map (applyInput base) $ zip [0..] patterns
  where applyInput state (i, pat) = (`rem` 10) . abs . sum $ zipWith (*) (drop i state) (drop i pat)

rev :: [Int] -> [Int]
rev num = inner 0 [] $ reverse num
  where inner acc xs [] = xs
        inner acc xs (n:ns) = let new = (acc + n) `mod` 10
                               in inner new (new : xs) ns

solveA :: [Int] -> [Int]
solveA base = take 8 . (!! 100) . scanl phase base $ repeat (pattern [0,1,0,-1])

solveB :: [Int] -> [Int]
solveB base = 
  let offset = foldl ((+) . (*10)) 0 $ take 7 base
      rest = drop offset (concat $ replicate 10000 base)
   in take 8 . (!! 100) $ iterate rev rest

main = do
  contents <- strToInts . head . lines <$> readFile "data/input-2019-16.txt"

  putStrLn $ concatMap show $ solveA contents
  putStrLn $ concatMap show $ solveB contents

-- 76795888
-- 84024125 
