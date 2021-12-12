-- This solution is slightly overpowered due to misreading the spec as every cave being able to be used twice
-- A more directy solution is included in `alternative.hs`

import           Data.Char       (isLower)
import           Data.Foldable   (find)
import           Data.List.Split (splitWhen)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

type Vertex = String
type Edge = (Vertex, Vertex)
type Case = [Edge]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readEdge . lines

readEdge :: String -> Edge
readEdge s = let [u, v] = splitWhen (== '-') s in (u, v)

showSoln :: Soln -> String
showSoln = unlines . return . show

-- Note that for the problem to be well defined there must not be infinite loops, and so must be no edges between big caves
-- Therefore we can remove large vertices so long as we replace them with edges between all their neighbours
-- These edges must still be counted as separate paths, however, so we must keep track of how many edges there are

-- Number of edges from one vertex to each of its neighbours
type AdjMap = Map Vertex (Map Vertex Int)

adjMap :: [Edge] -> AdjMap
adjMap es = Map.unionsWith (Map.unionWith (+)) $ map (\(u, v) -> Map.fromList [(u, Map.singleton v 1), (v, Map.singleton u 1)]) es

isSmall :: Vertex -> Bool
isSmall = isLower . head

reduceBigs :: AdjMap -> AdjMap
reduceBigs adj = Map.unionWith (Map.unionWith (+)) smalls cliques
  where
    (smalls, bigs) = Map.partitionWithKey (\k _ -> isSmall k) adj
    cliques = Map.fromListWith (Map.unionWith (+)) [ (v, c) | c <- Map.elems bigs, v <- Map.keys c ]

-- (current location, number of times each vertex has been visited)
type State = (Vertex, Map Vertex Int)

limitCombinations :: (Ord a) => Map a Int -> [Map a Int]
limitCombinations cs = go [Map.empty] $ Map.keys cs
  where
    go ms [] = ms
    go ms (k:ks) = go (ms ++ [ Map.insert k i m | i <- [1..(Map.findWithDefault 0 k cs)], m <- ms ]) ks

pathCounts :: Map Vertex Int -> Vertex -> AdjMap -> Map State Int
pathCounts limits src adj = dpt
  where
    dps = [ (v, s) | v <- Map.keys adj, s <- limitCombinations $ Map.adjust pred v $ limits ]
    dpt = Map.mapWithKey (\s _ -> dpf s) $ Map.fromList $ zip dps $ repeat ()
    dpf (v, s) | v == src = if Map.null s then 1 else 0
    dpf (v, s) = sum $ map rf ps
      where
        es = Map.findWithDefault Map.empty v adj
        ps = Set.elems $ Set.intersection (Map.keysSet s) $ Map.keysSet es
        rf p = (Map.findWithDefault 0 (p, dec p s) dpt) * (Map.findWithDefault 0 p es)
        dec = Map.alter (find (/= 0) . fmap pred)

numPathsWithLimits :: Map Vertex Int -> AdjMap -> Int
numPathsWithLimits limits = sum . Map.elems . Map.filterWithKey (\(v, s) _ -> v == "end" && Map.member "start" s) . pathCounts limits "start"

numPaths :: AdjMap -> Int
numPaths adj = (+) base $ sum $ map (\v -> (numPathsWithLimits (Map.adjust succ v cs) adj) - base) $ vs
  where
    base = numPathsWithLimits cs adj
    cs = Map.fromList $ zip (Map.keys adj) $ repeat 1
    vs = Map.keys $ Map.delete "start" $ Map.delete "end" adj

solve :: Case -> Soln
solve = numPaths . reduceBigs . adjMap
