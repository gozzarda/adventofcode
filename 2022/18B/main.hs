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

thrice :: (a -> b) -> (a, a, a) -> (b, b, b)
thrice f (x, y, z) = (f x, f y, f z)

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

dfscut :: Set Point -> Point -> Set Point
dfscut pts pt | Set.notMember pt pts = pts
dfscut pts pt = foldl' dfscut (Set.delete pt pts) adjs
  where
    adjs = map (thrice2 (+) pt) axes ++ map (thrice2 (-) pt) axes

bounds :: [Point] -> (Point, Point)
bounds pts = (l, u)
  where
    l = thrice minimum $ unzip3 pts
    u = thrice maximum $ unzip3 pts

enclosed :: Set Point -> Set Point
enclosed pts = dfscut air l'
  where
    (l, u) = bounds $ Set.elems pts
    l'@(xl, yl, zl) = thrice (subtract 1) l
    u'@(xu, yu, zu) = thrice (+ 1) u
    box = Set.fromList $ [(x, y, z) | x <- [xl .. xu], y <- [yl .. yu], z <- [zl .. zu]]
    air = Set.difference box pts

solve :: Case -> Soln
solve pts = surface pts''
  where
    pts' = Set.fromList pts
    pts'' = Set.union pts' $ enclosed pts'
