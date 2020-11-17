{-# LANGUAGE TupleSections #-}
module Lib
    ( someFunc
    , freqs
    ) where

import qualified Data.Map as Map
import           Data.Map   ( Map )
import Data.Foldable

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = Map.fromListWith (+) . map (,1) . toList

someFunc :: IO ()
someFunc = putStrLn "someFunc"
