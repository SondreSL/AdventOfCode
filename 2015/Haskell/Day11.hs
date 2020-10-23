{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Char
import           Data.List                      ( find )
import Data.Foldable

puzzleInput :: Text
puzzleInput = "cqjxjnds"

inc :: Text -> Text
inc t = case T.last t of
  'z' -> inc (T.init t) <> T.singleton 'a'
  c   -> T.init t <> T.singleton (chr (ord c + 1))

triples :: [Text]
triples = go "abc"
 where
  go :: Text -> [Text]
  go "xyz" = ["xyz"]
  go xs    = [xs] <> go (T.map (chr . (+ 1) . ord) xs)

pairs :: [Text]
pairs = [ T.singleton a <> T.singleton a | a <- ['a' .. 'z']]

good :: Text -> Bool
good t = and $ pair t : three t : (xs <*> pure t)
 where
  xs = [not . T.any (== 'i'), not . T.any (== 'l'), not . T.any (== 'o')]
  three t = or $ (`T.isInfixOf` t) <$> triples
  pair t = (>1) . length . filter id $ (`T.isInfixOf` t) <$> pairs

main :: IO ()
main = do
  print . head . filter good . iterate inc $ puzzleInput
  print . (!! 1) . filter good . iterate inc $ puzzleInput

-- "cqjxxyzz"
-- "cqkaabcc"
