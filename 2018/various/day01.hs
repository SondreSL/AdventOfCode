-- Works, but the second part is very slow.

parseFile = map (read :: String -> Int) . lines . filter (\x -> x /= '+')

solveA = foldr (+) 0 . parseFile

acc :: [Int] -> Int
acc xs =
    let 
    inner n seen [] = inner n seen xs
    inner n seen (y:ys) = if elem n seen
                            then n
                            else inner (n + y) (n:seen) ys
    in
    inner 0 [] xs

solveB = acc . parseFile
    

main :: IO ()
main = do
    -- Part 1
    contents <- readFile "day01.txt"
    print $ solveA contents
    print $ solveB contents

