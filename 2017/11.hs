import Data.Maybe
import Data.List.Extra
import Math.Geometry.Grid (distance, neighbour)
import Math.Geometry.Grid.Hexagonal2 (UnboundedHexGrid(..))
import Math.Geometry.Grid.HexagonalInternal2 (HexDirection(..))
import Text.ParserCombinators.Parsec

parseInput :: String -> [HexDirection]
parseInput = map (\x -> case x of
                          "nw" -> Northwest
                          "ne" -> Northeast
                          "n" -> North
                          "se" -> Southeast
                          "sw" -> Southwest
                          "s" -> South)
           . splitOn ","
           . init

move :: (Int, Int) -> HexDirection -> (Int, Int)
move pos = fromJust . neighbour UnboundedHexGrid pos

part1 :: String -> Int
part1 = distance UnboundedHexGrid (0,0) . foldl move (0,0) . parseInput

part2 :: String -> Int
part2 = maximum . map (distance UnboundedHexGrid (0,0)) . scanl move (0,0) . parseInput

main = do
  input <- readFile "data/11.in"
  print $ part1 input
  print $ part2 input

-- 705
-- 1469
