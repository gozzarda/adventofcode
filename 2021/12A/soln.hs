import           Data.Char       (isLower)
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
    cliques = Map.fromListWith (Map.unionWith (+)) [ (v, Map.delete v c) | c <- Map.elems bigs, v <- Map.keys c ]

-- (current location, previously visited set of (small) vertices)
type State = (Vertex, Set Vertex)

pathCounts :: Vertex -> AdjMap -> Map State Int
pathCounts src adj = dpt
  where
    dps = [ (v, s) | v <- Map.keys adj, s <- Set.elems $ Set.powerSet $ Set.delete v $ Map.keysSet adj ]
    dpt = Map.mapWithKey (\s _ -> dpf s) $ Map.fromList $ zip dps $ repeat ()
    dpf (v, s) | v == src = if Set.null s then 1 else 0
    dpf (v, s) = sum $ map rf ps
      where
        es = Map.findWithDefault Map.empty v adj
        ps = Set.elems $ Set.intersection s $ Map.keysSet es
        rf p = (Map.findWithDefault 0 (p, Set.delete p s) dpt) * (Map.findWithDefault 0 p es)

numPaths :: AdjMap -> Int
numPaths = sum . Map.elems . Map.filterWithKey (\(v, s) _ -> v == "end" && Set.member "start" s) . pathCounts "start"

solve :: Case -> Soln
solve = numPaths . reduceBigs . adjMap
