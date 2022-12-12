{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (liftA2)
import Data.Char (isUpper)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

type Height = Char

type Case = [[Height]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = lines

showSoln :: Soln -> String
showSoln = unlines . return . show

type Coord = (Int, Int)

gridToMap :: [[a]] -> Map Coord a
gridToMap xss = Map.fromList [((r, c), x) | (r, xs) <- zip [0 ..] xss, (c, x) <- zip [0 ..] xs]

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (ll, lr) (rl, rr) = (f ll rl, f lr rr)

steps :: Coord -> [Coord]
steps c = map (both2 (+) c) [(-1, 0), (0, -1), (0, 1), (1, 0)]

type Vert = Coord

bfs :: (Vert -> [Vert]) -> Vert -> Map Vert Int
bfs adj src = bfs' (Map.singleton src 0) 0 [src]
  where
    bfs' dm d [] = dm
    bfs' dm d q = bfs' dm' (d + 1) q'
      where
        (dm', q') = foldl f (dm, []) $ concatMap adj q
        f (m, s) v = if Map.notMember v m then (Map.insert v (d + 1) m, v : s) else (m, s)

neighbours :: Map Vert Height -> Vert -> [Vert]
neighbours hm u = filter (\v -> fromMaybe False $ liftA2 ((<=) . pred) (Map.lookup u hm) (Map.lookup v hm)) (steps u)

solve :: Case -> Soln
solve css = minimum $ Map.elems ads
  where
    cm = gridToMap css
    marks = Map.fromList $ map swap $ Map.assocs $ Map.filter isUpper cm
    hm = Map.map (\case 'S' -> 'a'; 'E' -> 'z'; h -> h) cm
    dm = bfs (neighbours hm) (marks ! 'E')
    ads = Map.intersection dm $ Map.filter (== 'a') hm
