-- https://adventofcode.com/2015/day/7
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List.Extra                ( splitOn )
import           Data.Word                      ( Word16 )
import           Data.Char                      ( isDigit )
import           Data.List                      ( find )
import           Data.Bits                     as B
import qualified Data.Text                     as T
import           Data.Text.Read
import           Data.Text                      ( Text )

import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as Map

type Val = Either Text Word16

parse :: [Text] -> Map Text Word16 -> Map Text Word16
parse str start = finalMap
 where
  finalMap = foldr (uncurry (Map.insertWith (flip const)) . go . T.splitOn " ") start str
  val s = if T.all isDigit s then either error fst $ decimal s else finalMap Map.! s
  go ["NOT", b, _, to]       = (to, B.complement (val b))
  go [a, "OR"    , b, _, to] = (to, val a .|. val b)
  go [a, "AND"   , b, _, to] = (to, val a .&. val b)
  go [a, "RSHIFT", b, _, to] = (to, B.shiftR (val a) (fromIntegral (val b)))
  go [a, "LSHIFT", b, _, to] = (to, B.shiftL (val a) (fromIntegral (val b)))
  go [a, _, to]              = (to, val a)

main :: IO ()
main = do
  input <- T.lines . T.pack <$> readFile "../data/07.in"

  -- Del 1
  let ans = parse input Map.empty Map.! "a"
  print ans

  -- Del 2
  let new = parse input (Map.singleton "b" ans)
  print $ new Map.! "a"
