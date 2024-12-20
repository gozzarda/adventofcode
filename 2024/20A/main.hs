module Main where

import Data.Array.Unboxed
import Data.Foldable (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

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

type Coord = (Int, Int)

saveLimit :: Int
saveLimit = 100

cheatDist :: Int
cheatDist = 2

-- Guaranteed to be a single path but BFS is no harder
solve :: Prob -> Soln
solve xss = length $ filter (>= saveLimit) $ mapMaybe shortcut cheats
  where
    nr = length xss
    nc = maximum $ map length xss
    rcBounds = ((0, 0), (nr, nc))
    rcxs = [((r, c), x) | (r, xs) <- zip [0 ..] xss, (c, x) <- zip [0 ..] xs]
    grid = array rcBounds rcxs :: UArray Coord Char
    isTrack = maybe False (/= '#') . (grid !?)
    goal = case filter ((== 'E') . snd) rcxs of
      [(rc, _)] -> rc
      _ -> error "No end point 'E' found in input"
    rcdm = bfs (adj isTrack) goal
    cheats = concatMap (cheatsFrom isTrack) (Map.keys rcdm)
    rcda = genArray rcBounds (rcdm Map.!?) :: Array Coord (Maybe Int)
    shortcut (u, v) =
      let d = manhattan u v
       in case (rcda !? u, rcda !? v) of
            (Just (Just ud), Just (Just vd)) -> find (> 0) $ Just $ ud - vd - d
            _ -> Nothing

cheatsFrom :: (Coord -> Bool) -> Coord -> [(Coord, Coord)]
cheatsFrom isTrack rc = map (rc,) $ filter isTrack $ pointsWithin cheatDist rc

pointsWithin :: Int -> Coord -> [Coord]
pointsWithin d (r, c) =
  [ (r + dr, c + dc)
    | dr <- [-d .. d],
      dc <- [(abs dr - d) .. (d - abs dr)]
  ]

manhattan :: Coord -> Coord -> Int
manhattan (lr, lc) (rr, rc) = abs (lr - rr) + abs (lc - rc)

type Vert = Coord

dirs :: [Coord]
dirs = [(-1, 0), (0, -1), (0, 1), (1, 0)]

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (lf, ls) (rf, rs) = (f lf rf, f ls rs)

vadd :: Coord -> Coord -> Coord
vadd = both2 (+)

adj :: (Vert -> Bool) -> Vert -> [Vert]
adj isTrack rc = filter isTrack (map (vadd rc) dirs)

bfs :: (Vert -> [Vert]) -> Vert -> Map Vert Int
bfs adj v = go Map.empty (Map.singleton v 0)
  where
    go dm udm | Map.null udm = dm
    go dm udm = go (Map.union dm udm) (Map.difference vdm dm)
      where
        vds = concatMap (\(u, d) -> map (,succ d) (adj u)) (Map.assocs udm)
        vdm = Map.fromList vds
