module Day25 where

import Intcode
    ( Intcode(Intcode),
      ProgramState(Wait, Halt, Input),
      input,
      output,
      run,
      untilInput,
      parseIntcode )
import Control.Lens ( (&), (.~) )

-- Actually play the game.
-- You need to hold the coin, cake, hologram and hypercube
-- to pass through the checkpoint in the south-east corner of the map.

play :: ProgramState -> IO ()
play (Halt ic) = print (Halt ic)
play (Input ic) = do
  print (Input ic)
  cmd <- getLine
  let new = Wait $ ic & input .~ (cmd ++ "\n") & output .~ []
  putStrLn "\n ========= \n"
  play new
play ic = play $ untilInput $ iterate run ic

main :: IO ()
main = do
  memory <- parseIntcode <$> readFile "../data/input-2019-25.txt"
  let ic = Intcode 0 0 [] [] memory
  play (Wait ic)

-- 4362
