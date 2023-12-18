module Main where

import Data.List (scanl1, uncons)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
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

-- (pos, True iff next move must be vertical)
type Vert = (Coord, Bool)

type Weight = Int

-- Maximum distance allowed in straight line
maxSteps :: Int
maxSteps = 3

neighbours :: Map Coord Weight -> Vert -> [(Vert, Weight)]
neighbours iwm (pos, vertical) = concatMap dirvws dirs
  where
    dirvws dir = take maxSteps $ zip vs ws
      where
        ps = tail $ iterate (addCoord dir) pos
        vs = map (\p -> (p, not vertical)) ps
        ws = scanl1 (+) $ takeWhileJust $ map (flip Map.lookup iwm) ps
    dirs = if vertical then [(-1, 0), (1, 0)] else [(0, -1), (0, 1)]

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust (Just x:mxs) = x : takeWhileJust mxs
takeWhileJust _ = []

dijkstras :: Map Coord Weight -> Coord -> Coord -> Maybe Weight
dijkstras iwm src dst = go (Map.empty) $ pqFromList [(0, (src, True)), (0, (src, False))]
  where
    go vdm dvpq = case pqPop dvpq of
      Nothing -> Nothing
      Just ((d, (p, _)), _) | p == dst -> Just d
      Just ((_, v), dvpq') | Map.member v vdm -> go vdm dvpq'
      Just ((d, v), dvpq') -> go (Map.insertWith min v d vdm) (pqPushes (map swap ns) dvpq')
        where
          ns = map (fmap (+ d)) $ filter (\(v, _) -> Map.notMember v vdm) $ neighbours iwm v

type PQueue k v = Map k [v]

pqEmpty :: Ord k => PQueue k v
pqEmpty = Map.empty

pqSingleton :: Ord k => k -> v -> PQueue k v
pqSingleton k v = Map.singleton k [v]

pqPush :: Ord k => k -> v -> PQueue k v -> PQueue k v
pqPush k v = Map.insertWith (++) k [v]

pqPushes :: Ord k => [(k, v)] -> PQueue k v -> PQueue k v
pqPushes kvs q = foldr (uncurry pqPush) q kvs

pqPop :: (Show k, Show v, Ord k) => PQueue k v -> Maybe ((k, v), PQueue k v)
pqPop q = case Map.lookupMin q of
  Nothing -> Nothing
  Just (k, []) -> pqPop q'
  Just (k, (v : _)) -> Just ((k, v), q')
  where
    q' = Map.updateMin (fmap snd . uncons) q

pqFromList :: Ord k => [(k, v)] -> PQueue k v
pqFromList = flip pqPushes pqEmpty
