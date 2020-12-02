
part1 :: undefined
part1 = undefined

part2 :: undefined
part2 = undefined

main = do
  input <- readFile "data/11.in"
  mapM_ print $ lines input

-- The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.
-- The second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.
-- The third floor contains a thulium-compatible microchip.
-- The fourth floor contains nothing relevant.
-- 
-- To move x items one floor up: 
--   2 moves per item
--   - 2 because the last two items take one trip
--   - 1 becauce the first item took one trip
--   = 2 * x - 3
-- 
-- 5 steps to get everything but T on lvl 2
-- 15 steps to move from 2 -> 3
-- 17 steps for 3 -> 4
--   = 37
-- 
-- -- Part 2
-- 
-- 13 for 1 -> 2
-- 23 for 2 -> 3
-- 25 for 3 -> 4
--   = 61
