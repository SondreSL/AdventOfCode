import Data.List
import Data.Maybe
import Data.Semigroup hiding (Sum)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

type Orbits = Map.Map String [String]

-- Parser
parser :: String -> Orbits
parser = Map.fromListWith (++) . map parse' . lines
  where parse' = either (error "Bad parse") tuple . parse name "Orbits"
        name = sepBy (many1 alphaNum) (char ')')
        tuple [a,b] = (a,[b])

-- Logic
data Sum = Combined Int | Single Int | Null
  deriving Show

instance Semigroup Sum where
  Null <> Null = Null
  Single a <> Single b = Combined (a + b)
  Null <> Single a = Single a
  Single a <> Null = Single a
  Combined a <> Null = Combined a
  Null <> Combined a = Combined a

instance Monoid Sum where
  mempty = Null

cntOrbits :: Orbits -> Int -> String -> Int
cntOrbits orbits n str = n + (sum $ map (cntOrbits orbits (n + 1)) $ Map.findWithDefault [] str orbits)

subtree :: Orbits -> String -> String -> String -> Sum
subtree m a b curr = toSum $ Map.lookup curr m
  where toSum Nothing = Null
        toSum (Just xs) 
          | elem a xs && elem b xs = Combined 0 
          | elem a xs || elem b xs = Single 0 
          | otherwise = foldMap (step . subtree m a b) xs 
        step (Single a) = Single (a + 1)  
        step a = a

solveA :: Orbits -> Int
solveA m = cntOrbits m 0 "COM"

solveB :: Orbits -> Int
solveB m = case subtree m "YOU" "SAN" "COM" of
             (Combined ans) -> ans
             Null -> error "Invalid tree or input"

main = do
  orbits <- parser <$> readFile "data/input-2019-6.txt"
  print $ solveA orbits
  print $ solveB orbits

-- 295936
-- 457
