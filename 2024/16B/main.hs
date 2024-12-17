module Main where

import Data.Array.Unboxed
import Data.Bifunctor (bimap)
import Data.Containers.ListUtils (nubOrd)
import Data.List (singleton)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

type Prob = [[Char]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = lines

solve :: Prob -> Soln
solve xss = Set.size $ Set.map fst vs
  where
    nr = length xss
    nc = maximum $ map length xss
    rcBounds = ((0, 0), (nr - 1, nc - 1))
    rcxs = [((r, c), x) | (r, xs) <- zip [0 ..] xss, (c, x) <- zip [0 ..] xs]
    grid = array rcBounds rcxs
    roots = map (\(rc, _) -> (rc, (0, 1))) $ filter ((== 'S') . snd) rcxs
    vpsm = dijkstras (adj grid) roots
    isGoal (rc, _) = grid !? rc == Just 'E'
    goals = filter isGoal $ Map.keys vpsm
    vs = dfs (\v -> Map.findWithDefault [] v vpsm) goals

type Coord = (Int, Int)

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (lf, ls) (rf, rs) = (f lf rf, f ls rs)

vadd :: Coord -> Coord -> Coord
vadd = both2 (+)

type Vert = (Coord, Coord)

type Dist = Int

adj :: UArray Coord Char -> Vert -> [(Dist, Vert)]
adj grid (rc, drc) = catMaybes [moveForward, turnLeft, turnRight]
  where
    moveForward = case grid !? vadd rc drc of
      Nothing -> Nothing
      Just '#' -> Nothing
      _ -> Just (1, (vadd rc drc, drc))
    (dr, dc) = drc
    turnLeft = Just (turnCost, (rc, (-dc, dr)))
    turnRight = Just (turnCost, (rc, (dc, -dr)))
    turnCost = if grid !? rc == Just 'E' then 0 else 1000

type Edge = (Vert, Vert)

dijkstras :: (Vert -> [(Dist, Vert)]) -> [Vert] -> Map Vert [Vert]
dijkstras adj roots = go Map.empty (Map.singleton 0 $ zip roots roots)
  where
    go :: Map Vert [Vert] -> Map Dist [Edge] -> Map Vert [Vert]
    go vpsm pq = case Map.minViewWithKey pq of
      Nothing -> vpsm
      Just ((d, ups), pq') -> go vpsm' pq''
        where
          upsm = Map.fromListWith (++) $ map (fmap singleton) ups
          vpsm' = Map.union vpsm upsm
          us = Map.keys $ Map.difference upsm vpsm
          wvus = concatMap (\u -> map (fmap (,u)) (adj u)) us
          dvus = map (bimap (d +) singleton) wvus
          pq'' = Map.unionWith (++) pq' $ Map.fromListWith (++) dvus

dfs :: (Vert -> [Vert]) -> [Vert] -> Set Vert
dfs adj = foldl go Set.empty
  where
    go seen u | Set.member u seen = seen
    go seen u = foldl go (Set.insert u seen) (adj u)
