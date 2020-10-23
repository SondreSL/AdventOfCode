{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           Data.Maybe
import           Data.Scientific
import           GHC.Generics
import qualified Data.Vector                   as V
import qualified Data.ByteString.Lazy          as BS
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Char8         as B8
import qualified Data.HashMap.Strict           as HM

nums :: ([Value] -> Bool) -> Value -> Int
nums f (Object o) = if f (HM.elems o) then 0 else sum (map (nums f) (HM.elems o))
nums f (Array  a) = sum (V.map (nums f) a)
nums f (String a) = 0
nums f (Number a) = fromJust (toBoundedInteger a)
nums f (Bool   a) = 0
nums f Null       = 0

main :: IO ()
main = do
  input <- BS.readFile "../data/12.in"
  let Just json = decode input :: Maybe Value
  print $ nums (const False) json
  print $ nums (String "red" `elem`) json
