module Main where

simplify :: String -> Int
simplify txt = length txt - memChars txt
 where
  memChars []                           = 0
  memChars ('"'                   : xs) = memChars xs
  memChars ('\\'           : '\\' : xs) = 1 + memChars xs
  memChars ('\\' : 'x' : _ : _    : xs) = 1 + memChars xs
  memChars ('\\'           : '"'  : xs) = 1 + memChars xs
  memChars (_                     : xs) = 1 + memChars xs

encode :: String -> Int
encode str = length (show str) - length str

fileDiff :: (String -> Int) -> [String] -> Int
fileDiff f = sum . map f

main :: IO ()
main = do
  input <- lines <$> readFile "../data/08.in"
  print $ fileDiff simplify input
  print $ fileDiff encode input
