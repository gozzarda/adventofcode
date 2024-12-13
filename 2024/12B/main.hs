module Main where

import Data.Array.Unboxed
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

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

dirs :: [(Int, Int)]
dirs = [(-1, 0), (0, -1), (0, 1), (1, 0)]

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (lf, ls) (rf, rs) = (f lf rf, f ls rs)

solve :: Prob -> Soln
solve xss = sum $ map cost comps
  where
    nr = length xss
    nc = maximum $ map length xss
    rcBounds = ((0, 0), (nr - 1, nc - 1))
    grid = listArray rcBounds (concat xss) :: UArray (Int, Int) Char
    neighbours rc = case grid !? rc of
      Nothing -> []
      Just x -> filter ((== Just x) . (grid !?)) $ map (both2 (+) rc) dirs
    comps = dfsComponents neighbours (indices grid)
    cost comp = length comp * numSides comp

-- Components in an undirected graph
dfsComponents :: (Ord a) => (a -> [a]) -> [a] -> [[a]]
dfsComponents adj vs = filter (not . null) $ snd $ foldl go (Set.empty, []) vs
  where
    go (seen, comps) v = (: comps) <$> dfs (seen, []) v
    dfs (seen, ord) v | Set.member v seen = (seen, ord)
    dfs (seen, ord) v = (v :) <$> foldl dfs (Set.insert v seen, ord) (adj v)

-- Number of sides of component made up of given cells
numSides :: [(Int, Int)] -> Int
numSides rcs = Set.size edges - sum (Map.map Set.size straights)
  where
    cellVerts (r, c) = [(r, c), (r + 1, c), (r + 1, c + 1), (r, c + 1)]
    cellEdges rc = let vs = cellVerts rc in zip vs (drop 1 $ cycle vs)
    edges =
      let es = concatMap cellEdges rcs
       in Set.difference (Set.fromList es) (Set.fromList $ map swap es)
    uvdm = Map.fromSet (Set.singleton . uncurry (both2 subtract)) edges
    uodm = Map.mapKeysWith Set.union fst uvdm
    vidm = Map.mapKeysWith Set.union snd uvdm
    straights = Map.intersectionWith Set.intersection uodm vidm
