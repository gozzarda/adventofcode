module Main where

import Data.List (uncons)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Tuple (swap)

type Prob = [[Int]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = (map . map) (read . (: [])) . lines

solve :: Prob -> Soln
solve tss = fromJust $ dijkstras iwm src dst
  where
    iwm = Map.fromList $ concat $ zipWith zip [[(r, c) | c <- [0 ..]] | r <- [0 ..]] tss
    src = fst $ Map.findMin iwm
    dst = fst $ Map.findMax iwm

-- (row, col)
type Coord = (Int, Int)

addCoord :: Coord -> Coord -> Coord
addCoord (lr, lc) (rr, rc) = (lr + rr, lc + rc)

-- (pos, dir, steps until forced turn)
type Vert = (Coord, Coord, Int)

type Weight = Int

-- Maximum distance allowed in straight line
stepsLimit :: Int
stepsLimit = 3

neighbours :: Map Coord Weight -> Vert -> [(Vert, Weight)]
neighbours iwm (pos, dir@(dr, dc), steps) = catMaybes $ map (\v -> fmap ((,) v) $ weight v) $ catMaybes $ map next dirs
  where
    dirs = [(-1, 0), (0, -1), (0, 1), (1, 0)]
    next d = case d of
      d | d == dir -> if steps > 0 then Just (addCoord pos d, d, steps - 1) else Nothing
      (dr', dc') | dr' == (-dr) && dc' == (-dc) -> Nothing
      d -> Just (addCoord pos d, d, stepsLimit - 1)
    weight (pos, _, _) = Map.lookup pos iwm

dijkstras :: Map Coord Weight -> Coord -> Coord -> Maybe Weight
dijkstras iwm src dst = go (Map.empty) (pqSingleton 0 (src, (0, 0), 0))
  where
    go vdm dvpq = case pqPop dvpq of
      Nothing -> Nothing
      Just ((d, (p, _, _)), _) | p == dst -> Just d
      Just ((_, v), dvpq') | Map.member v vdm -> go vdm dvpq'
      Just ((d, v), dvpq') -> go (Map.insertWith min v d vdm) (pqPushes (map swap ns) dvpq')
        where
          ns = map (fmap (+ d)) $ filter (\(v, _) -> Map.notMember v vdm) $ neighbours iwm v

type PQueue k v = Map k [v]

pqSingleton :: Ord k => k -> v -> PQueue k v
pqSingleton k v = Map.singleton k [v]

pqPush :: Ord k => k -> v -> PQueue k v -> PQueue k v
pqPush k v = Map.insertWith (++) k [v]

pqPushes :: Ord k => [(k, v)] -> PQueue k v -> PQueue k v
pqPushes kvs q = foldr (uncurry pqPush) q kvs

pqPop :: Ord k => PQueue k v -> Maybe ((k, v), PQueue k v)
pqPop q = case Map.lookupMin q of
  Nothing -> Nothing
  Just (k, []) -> pqPop q'
  Just (k, (v : _)) -> Just ((k, v), q')
  where
    q' = Map.updateMin (fmap snd . uncons) q
