module Main where

import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

type Vert = String

type Edge = (Vert, Vert)

type Prob = [Edge]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map readEdge . lines

readEdge :: String -> Edge
readEdge s = let [u, v] = splitBy (== '-') s in (u, v)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p xs = case break p $ dropWhile p xs of
  ([], _) -> []
  (pref, suff) -> pref : splitBy p suff

solve :: Prob -> Soln
solve uvl = length $ filter (any (isPrefixOf "t")) $ Set.elems tris
  where
    el = uvl ++ map swap uvl
    es = Set.fromList el
    adjm = Map.fromListWith Set.union $ map (fmap Set.singleton) el
    trisf (u, vs) = Set.map (\(v, t) -> Set.fromList [u, v, t]) vts
      where
        vts = Set.intersection es $ Set.cartesianProduct vs vs
    tris = Set.unions $ map trisf $ Map.assocs adjm
