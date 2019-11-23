-- Advent of Code 2018 - Day 2
-- www.github.com/SondreSL

-- Part 1

count :: String -> Int
count (x:xs) = length (filter (x==) xs) + 1

findN :: Int -> String -> Int
findN _ "" = 0
findN n xs = if count xs == n
                then 1
                else findN n (tail xs)

solveA xs = res2 xs * res3 xs
    where
        res2 = sum . map (findN 2) . lines
        res3 = sum . map (findN 3) . lines

-- Not the correct answer. Off by 35, 8750 should be 8715.

-- Part 2
    


-- Main
main :: IO ()
main = do
    contents <- readFile "day02.txt"
    print $ solveA contents
