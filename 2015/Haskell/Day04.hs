module Main where

import Data.Hash.MD5
import Data.List (find)

input :: String
input = "ckczppom"

leadingZeroes :: Int -> String -> Maybe (Int, String)
leadingZeroes n str = find (all (=='0') . take n . snd) . zip [1..] . map (md5s . Str . (str++) . show) $ [1..]

main :: IO ()
main = do
  print $ leadingZeroes 5 input
  print $ leadingZeroes 6 input

-- Just (117946,"00000fe1c139a2c710e9a5c03ec1af03")
-- Just (3938038,"00000028023e3b4729684757f8dc3fbf")
