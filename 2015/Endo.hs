
import Data.Monoid

left :: Char -> (Int -> Int)
left '(' = (+1)
left ')' = subtract 1

-- Endo a :: a -> a

part1 = flip appEndo 0 . foldMap (Endo . left)

part2 = undefined

main :: IO ()
main = do
  input <- init <$> readFile "data/01.in"
  print $ part1 input
  -- print $ part2 input
