module Main where

import Data.Array.Unboxed
import Data.Bifunctor (bimap)
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
solve xss = minimum $ Map.filterWithKey (const . isGoal) vdm
  where
    nr = length xss
    nc = maximum $ map length xss
    rcBounds = ((0, 0), (nr - 1, nc - 1))
    rcxs = [((r, c), x) | (r, xs) <- zip [0 ..] xss, (c, x) <- zip [0 ..] xs]
    grid = array rcBounds rcxs
    roots = map (\(rc, _) -> (rc, (0, 1))) $ filter ((== 'S') . snd) rcxs
    isGoal (rc, _) = grid !? rc == Just 'E'
    vdm = dijkstras (adj grid) roots

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
    turnLeft = Just (1000, (rc, (-dc, dr)))
    turnRight = Just (1000, (rc, (dc, -dr)))

dijkstras :: (Vert -> [(Dist, Vert)]) -> [Vert] -> Map Vert Dist
dijkstras adj roots = go Map.empty (Map.singleton 0 $ Set.fromList roots)
  where
    go udm pq = case Map.minViewWithKey pq of
      Nothing -> udm
      Just ((d, us), pq') -> go udm' pq''
        where
          us' = Set.filter (`Map.notMember` udm) us
          udm' = Map.union udm $ Map.fromSet (const d) us'
          wvs = concatMap (filter (\(_, v) -> Map.notMember v udm') . adj) us'
          dvs = map (bimap (d +) Set.singleton) wvs
          pq'' = Map.unionWith Set.union pq' $ Map.fromListWith Set.union dvs
