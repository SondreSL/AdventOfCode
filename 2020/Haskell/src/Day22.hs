module Day22 where

import Data.Foldable (toList)
import Data.List.Extra (splitOn)
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (both)
import Lib (tuple)

score :: Foldable t => t Int -> Int
score = sum . zipWith (*) [1 ..] . reverse . toList

play :: Seq Int -> Seq Int -> Int
play as Empty = score as
play Empty bs = score bs
play (a :<| xs) (b :<| bs)
    | a > b = play (xs |> a |> b) bs
    | b > a = play xs (bs |> b |> a)

play' :: Set (Seq Int, Seq Int) -> Seq Int -> Seq Int -> Either Int Int
play' _ as Empty = Left $ score as
play' _ Empty bs = Right $ score bs
play' seen x@(a :<| as) y@(b :<| bs)
    | Set.member (x, y) seen = Left $ score x
    | a > Seq.length as || b > Seq.length bs =
        case compare a b of
            LT -> play' (Set.insert (x, y) seen) as             (bs |> b |> a)
            GT -> play' (Set.insert (x, y) seen) (as |> a |> b) bs
    | otherwise =
        case play' Set.empty (Seq.take a as) (Seq.take b bs) of
            Left  _ -> play' (Set.insert (x, y) seen) (as |> a |> b) bs
            Right _ -> play' (Set.insert (x, y) seen) as             (bs |> b |> a)

main :: IO ()
main = do
    (as, bs) <- parseInput <$> readFile "../data/day22.in"
    print $ play as bs
    print $ either id id $ play' Set.empty as bs

-- 33434
-- 31657

parseInput :: String -> (Seq Int, Seq Int)
parseInput = both Seq.fromList . tuple . map (map read . tail . lines) . splitOn "\n\n"
