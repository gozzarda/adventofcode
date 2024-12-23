module Main where

import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

type Vert = String

type Edge = (Vert, Vert)

type Prob = [Edge]

type Soln = Set Vert

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . intercalate "," . Set.elems

readProb :: String -> Prob
readProb = map readEdge . lines

readEdge :: String -> Edge
readEdge s = let [u, v] = splitBy (== '-') s in (u, v)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p xs = case break p $ dropWhile p xs of
  ([], _) -> []
  (pref, suff) -> pref : splitBy p suff

solve :: Prob -> Soln
solve uvl = maximumClique vs adjm
  where
    el = uvl ++ map swap uvl
    es = Set.fromList el
    vs = Set.fromList $ map fst el
    adjm = Map.fromListWith Set.union $ map (fmap Set.singleton) el

maximumClique :: Set Vert -> Map Vert (Set Vert) -> Set Vert
maximumClique vs adjm = snd $ dfs (Set.empty, Set.empty) (Set.empty, vs)
  where
    adjs u = Map.findWithDefault Set.empty u adjm
    dfs (seen, best) (us, _) | Set.member us seen = (seen, best)
    dfs (seen, best) (us, vs) = foldl dfs sb uvs
      where
        sb = (Set.insert us seen, maxOn Set.size best us)
        uvs = map uvf (Set.elems vs)
        uvf v = (Set.insert v us, Set.intersection vs (adjs v))

maxOn :: (Ord b) => (a -> b) -> a -> a -> a
maxOn f x y = if f x <= f y then y else x
