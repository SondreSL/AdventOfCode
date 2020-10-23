
import Data.Monoid
import qualified Data.Set as Set
import           Data.Set   ( Set )
import Control.Lens

toTuple :: Char -> (Sum Int, Sum Int)
toTuple '^' = (Sum 1, Sum 0)
toTuple 'v' = (Sum (-1), Sum 0)
toTuple '<' = (Sum 0, Sum (-1))
toTuple '>' = (Sum 0, Sum 1)

findHouses :: [(Sum Int, Sum Int)] -> Set (Sum Int, Sum Int)
findHouses = Set.fromList . scanl (<>) (Sum 0, Sum 0)

main :: IO ()
main = do
  contents <- map toTuple . init <$> readFile "data/03.in"
  let path = findHouses contents
  print . length $ path
  let f p = toListOf (traversed . indices p) contents
  print . length $ Set.union (findHouses (f even)) (findHouses (f odd))
