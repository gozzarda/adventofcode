module Main where

import Control.Monad (foldM)
import Data.Ix (range)
import Data.Set (Set)
import qualified Data.Set as Set

type Coord = (Int, Int)

type Path = [Coord]

type Case = [Path]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readPath . lines

readPath :: String -> Path
readPath = map readCoord . filter (/= "->") . words

readCoord :: String -> Coord
readCoord s = let (xw, ',' : yw) = span (/= ',') s in (read xw, read yw)

showSoln :: Soln -> String
showSoln = unlines . return . show

dfs :: Int -> Set Coord -> Coord -> Set Coord
dfs lim pts (x, y) | y > lim = Set.insert (x, y) pts
dfs lim pts (x, y) | Set.member (x, y) pts = pts
dfs lim pts (x, y) = foldl (dfs lim) (Set.insert (x, y) pts) adjs
  where
    adjs = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

insertPaths :: Set Coord -> [Path] -> Set Coord
insertPaths = foldl insertPath
  where
    insertPath pts p = foldl insertSeg pts $ zip p $ tail p
    insertSeg pts (l, r) = foldl (flip Set.insert) pts $ range (min l r, max l r)

solve :: Case -> Soln
solve ps = Set.size termState - Set.size initState
  where
    initState = insertPaths Set.empty ps
    lim = maximum $ map snd $ concat ps
    termState = dfs lim initState (500, 0)
