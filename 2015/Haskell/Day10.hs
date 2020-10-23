{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow
import Data.List (group)
import qualified Data.Text as T
import           Data.Text   ( Text )

puzzleInput :: Text
puzzleInput = "3113322113"

lookAndSay :: Text -> Text
lookAndSay = T.concat . map ((\(l, c) -> T.pack (show l ++ [c])) . (T.length &&& T.head)) . T.group

play :: Int -> Text -> Int
play n = T.length . (!! n) . iterate lookAndSay

main :: IO ()
main = do
  print $ play 40 puzzleInput
  print $ play 50 puzzleInput
