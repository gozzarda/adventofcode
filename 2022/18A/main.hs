module Main where

import Data.Function (on)
import Data.List (foldl', groupBy)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

type Point = (Int, Int, Int)

type Case = [Point]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readPoint . lines

readPoint :: String -> Point
readPoint s = let [x, y, z] = map read $ splitWhen (== ',') s in (x, y, z)

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f = filter (not . f . head) . groupBy ((==) `on` f)

showSoln :: Soln -> String
showSoln = unlines . return . show

thrice2 :: (a -> b -> c) -> (a, a, a) -> (b, b, b) -> (c, c, c)
thrice2 f (lx, ly, lz) (rx, ry, rz) = (f lx rx, f ly ry, f lz rz)

axes :: [Point]
axes = [(1, 0, 0), (0, 1, 0), (0, 0, 1)]

surface :: Set Point -> Int
surface pts = ncubes * 6 - nadjs * 2
  where
    ncubes = Set.size pts
    offs = map (flip Set.map pts . thrice2 (+)) axes
    adjs = map (Set.intersection pts) offs
    nadjs = sum $ map Set.size adjs

solve :: Case -> Soln
solve = surface . Set.fromList
