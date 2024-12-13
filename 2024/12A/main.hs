module Main where

import Data.Array.Unboxed
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
    cost comp = area * peri
      where
        area = length comp
        peri = (4 * area) - length (concatMap neighbours comp)

-- Components in an undirected graph
dfsComponents :: (Ord a) => (a -> [a]) -> [a] -> [[a]]
dfsComponents adj vs = filter (not . null) $ snd $ foldl go (Set.empty, []) vs
  where
    go (seen, comps) v = (: comps) <$> dfs (seen, []) v
    dfs (seen, ord) v | Set.member v seen = (seen, ord)
    dfs (seen, ord) v = (v :) <$> foldl dfs (Set.insert v seen, ord) (adj v)
