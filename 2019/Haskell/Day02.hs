import Data.List
import Text.ParserCombinators.Parsec
import qualified Data.Vector as V

-- I preprossesed the file to remove commas and allow for simple splitting
-- using words

strToInts :: String -> V.Vector Int
strToInts = V.fromList . map read . words

solveA :: V.Vector Int -> Int
solveA = undefined

solveB :: undefined
solveB = undefined

main = do
  contents <- strToInts <$> readFile "../data/input-2019-2.txt"
  print $ contents
  print 10
