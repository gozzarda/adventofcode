module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

data Tile = Path | Forest | East | North | West | South deriving (Ord, Eq)

type Prob = [[Tile]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = (map . map) readTile . lines

readTile :: Char -> Tile
readTile '.' = Path
readTile '#' = Forest
readTile '>' = East
readTile '^' = North
readTile '<' = West
readTile 'v' = South

solve :: Prob -> Soln
solve tss = fromJust $ longestPath g' src dst
  where
    its = concat $ zipWith zip [[(r, c) | c <- [0 ..]] | r <- [0 ..]] tss
    g = makeGraph its
    vs = graphVerts g
    src = Set.findMin vs
    dst = Set.findMax vs
    protected = Set.fromList [src, dst]
    g' = contract protected g

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (ll, lr) (rl, rr) = (f ll rl, f lr rr)

type Vec2 = (Int, Int)

(.+.) :: Vec2 -> Vec2 -> Vec2
(.+.) = both2 (+)

type Vert = Vec2

type Edge = (Vert, Vert)

type Weight = Int

type Graph = (Map Vert (Set Vert, Set Vert), Map Edge Weight)

graphEmpty :: Graph
graphEmpty = (Map.empty, Map.empty)

graphVerts :: Graph -> Set Vert
graphVerts (viom, _) = Map.keysSet viom

graphEdges :: Graph -> Set Edge
graphEdges (_, ewm) = Map.keysSet ewm

graphNumEdges :: Graph -> Int
graphNumEdges (_, ewm) = Map.size ewm

graphVertIOs :: Vert -> Graph -> (Set Vert, Set Vert)
graphVertIOs u (viom, _) = Map.findWithDefault (Set.empty, Set.empty) u viom

graphVertIOEdges :: Vert -> Graph -> (Set Edge, Set Edge)
graphVertIOEdges u g = (ius, uos)
  where
    (is, os) = graphVertIOs u g
    ius = Set.map (\i -> (i, u)) is
    uos = Set.map (\o -> (u, o)) os

graphVertEdges :: Vert -> Graph -> Set Edge
graphVertEdges u g = uncurry Set.union $ graphVertIOEdges u g

graphVertOutWeights :: Vert -> Graph -> Map Vert Weight
graphVertOutWeights u g@(_, ewm) = Map.mapKeys snd $ Map.restrictKeys ewm es
  where
    (_, es) = graphVertIOEdges u g

graphEdgeWeight :: Edge -> Graph -> Weight
graphEdgeWeight e (_, ewm) = Map.findWithDefault (error $ show e ++ " not in graph") e ewm

graphInsertVert :: Vert -> Graph -> Graph
graphInsertVert u (viom, ewm) = (Map.insertWith const u (Set.empty, Set.empty) viom, ewm)

graphDeleteVert :: Vert -> Graph -> Graph
graphDeleteVert u g@(viom, ewm) = foldr graphDeleteEdge (Map.delete u viom, ewm) $ graphVertEdges u g

graphInsertEdge :: Edge -> Weight -> Graph -> Graph
graphInsertEdge e@(u, v) w (viom, ewm) = (viom', ewm')
  where
    insert u ios = Map.alter (Just . foldr (both2 Set.union) ios) u
    viom' = insert v (Set.singleton u, Set.empty) $ insert u (Set.empty, Set.singleton v) viom
    ewm' = Map.insert e w ewm

graphDeleteEdge :: Edge -> Graph -> Graph
graphDeleteEdge e@(u, v) (viom, ewm) = (viom', ewm')
  where
    viom' = Map.adjust (\(is, os) -> (is, Set.delete v os)) u $ Map.adjust (\(is, os) -> (Set.delete u is, os)) v $ viom
    ewm' = Map.delete e ewm

tileDeltas :: Tile -> [Vec2]
tileDeltas Path = [(-1, 0), (0, -1), (0, 1), (1, 0)]
tileDeltas East = [(0, 1)]
tileDeltas North = [(-1, 0)]
tileDeltas West = [(0, -1)]
tileDeltas South = [(1, 0)]

makeGraph :: [(Vert, Tile)] -> Graph
makeGraph its = foldl insert graphEmpty edges
  where
    insert g e = graphInsertEdge e 1 g
    vtm = Map.filter (/= Forest) $ Map.fromList its
    neighbours u t = filter (`Map.member` vtm) $ map (u .+.) $ tileDeltas t
    edges = concatMap (\(u, vs) -> map ((,) u) vs) $ Map.assocs $ Map.mapWithKey neighbours vtm

contractVert :: Vert -> Graph -> Graph
contractVert u g = if safe && better then g' else g
  where
    (ts, vs) = graphVertIOs u g
    tvs = Set.fromList [(t, v) | t <- Set.elems ts, v <- Set.elems $ Set.delete t vs]
    wf (t, v) = graphEdgeWeight (t, u) g + graphEdgeWeight (u, v) g
    g' = foldr (\e -> graphInsertEdge e (wf e)) (graphDeleteVert u g) tvs
    safe = ts == vs && Set.size ts == 2 && (Set.null $ Set.intersection tvs $ graphEdges g)
    better = Set.size tvs <= Set.size ts + Set.size vs

contract :: Set Vert -> Graph -> Graph
contract protected = go
  where
    go g = if graphNumEdges g' < graphNumEdges g then go g' else g'
      where
        g' = foldr contractVert g $ Set.difference (graphVerts g) protected

longestPath :: Graph -> Vert -> Vert -> Maybe Weight
longestPath g src dst = go Set.empty src
  where
    go _ u | u == dst = Just 0
    go seen u = Map.foldl ((Just .) . maybe id max) Nothing $ Map.mapMaybeWithKey (\v w -> fmap (+ w) $ go (Set.insert u seen) v) $ Map.withoutKeys (graphVertOutWeights u g) seen
