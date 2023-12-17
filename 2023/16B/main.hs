module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
solve tss = maximum ns
  where
    grid = zipWith zip [[(r, c) | c <- [0 ..]] | r <- [0 ..]] tss
    ctm = Map.fromList $ concat grid
    iss = (map . map) fst grid
    ls = zip (map head iss) (repeat (0, 1))
    rs = zip (map last iss) (repeat (0, -1))
    ts = zip (head iss) (repeat (1, 0))
    bs = zip (last iss) (repeat (-1, 0))
    ns = map (Set.size . Set.map fst . bfs ctm) (ls ++ rs ++ ts ++ bs)

-- (row, col)
type Coord = (Int, Int)

addCoord :: Coord -> Coord -> Coord
addCoord (lr, lc) (rr, rc) = (lr + rr, lc + rc)

-- (pos, dir)
type Vert = (Coord, Coord)

neighbours :: Map Coord Char -> Vert -> [Vert]
neighbours ctm (pos, dir@(dr, dc)) = filter inBounds $ case Map.lookup pos ctm of
  Just '.' -> [(addCoord pos dir, dir)]
  Just '/' -> let dir' = (-dc, -dr) in [(addCoord pos dir', dir')]
  Just '\\' -> let dir' = (dc, dr) in [(addCoord pos dir', dir')]
  Just '|' -> case dc of
    0 -> [(addCoord pos dir, dir)]
    _ -> map (\d -> (addCoord pos d, d)) [(-1, 0), (1, 0)]
  Just '-' -> case dr of
    0 -> [(addCoord pos dir, dir)]
    _ -> map (\d -> (addCoord pos d, d)) [(0, -1), (0, 1)]
  where
    inBounds (pos, _) = Map.member pos ctm

bfs :: Map Coord Char -> Vert -> Set Vert
bfs ctm root = go (Set.singleton root) (Set.singleton root)
  where
    go vis vs | Set.null vs = vis
    go vis vs = go (Set.union vis ns) ns
      where
        ns = Set.difference (Set.fromList $ concatMap (neighbours ctm) vs) vis
