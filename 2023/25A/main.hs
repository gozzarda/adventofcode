module Main where

import Data.Bifunctor (bimap)
import Data.Char (isLower)
import Data.List (unfoldr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP

type Name = String

type Prob = [(Name, [Name])]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = doReadP' $ parseProb <* eof

doReadP' :: ReadP a -> String -> a
doReadP' p = fromMaybe (error "Parsing failed") . doReadP p

doReadP :: ReadP a -> String -> Maybe a
doReadP p = fmap fst . listToMaybe . readP_to_S p

parseName :: ReadP Name
parseName = munch1 isLower

parseLine :: ReadP (Name, [Name])
parseLine = do
  u <- parseName
  char ':'
  skipSpaces
  vs <- sepBy1 parseName skipSpaces
  return (u, vs)

parseProb :: ReadP Prob
parseProb = endBy parseLine skipSpaces

solve :: Prob -> Soln
solve = uncurry (*) . bimap Set.size Set.size . snd . minCut . makeGraph

type Vert = Name

type Edge = (Vert, Vert)

type Weight = Int

type Graph = Map Vert (Map Vert Weight)

makeGraph :: [(Vert, [Vert])] -> Graph
makeGraph uvss = Map.fromListWith Map.union $ concat [[(u, Map.singleton v 1), (v, Map.singleton u 1)] | (u, vs) <- uvss, v <- vs]

type MetaVert = Set Vert

type MetaEdge = (MetaVert, MetaVert)

type MetaGraph = Map MetaVert (Map MetaVert Weight)

makeMetaGraph :: Graph -> MetaGraph
makeMetaGraph = Map.map (Map.mapKeys Set.singleton) . Map.mapKeys (Set.singleton)

contractMetaEdge :: MetaEdge -> MetaGraph -> MetaGraph
contractMetaEdge (s, t) = combineMetaVerts . Map.map combineWeights
  where
    st = Set.union s t
    deleteMetaVerts = Map.delete s . Map.delete t
    findWeight m = let w = Map.findWithDefault 0 s m + Map.findWithDefault 0 t m in if w == 0 then Nothing else Just w
    combineWeights m = maybe id (Map.insert st) (findWeight m) (deleteMetaVerts m)
    findMetaVert = Map.findWithDefault Map.empty
    combineMetaVerts g = Map.insert st (Map.delete st $ Map.unionWith (+) (findMetaVert s g) (findMetaVert t g)) $ deleteMetaVerts g

minCutPhase :: MetaGraph -> Maybe (Weight, MetaEdge, (Set Vert, Set Vert))
minCutPhase g | Map.size g <= 1 = Nothing
minCutPhase g = Just $ go wm pq a
  where
    (a, awm) = Map.findMin g
    wm = Map.delete a $ Map.union awm $ Map.map (const 0) g
    pq = Set.fromList $ map swap $ Map.assocs wm
    go wm pq s = if Map.notMember t wm then go wm pq' s else if Map.null wm' then (w, (s, t), (sc, t)) else go wm' pq'' t
      where
        ((w, t), pq') = Set.deleteFindMax pq
        twm = Map.intersectionWith (+) wm $ g Map.! t
        wm' = Map.delete t $ Map.union twm wm
        pq'' = Set.union pq' $ Set.fromList $ map swap $ Map.assocs twm
        vs = Set.foldr Set.union Set.empty $ Map.keysSet g
        sc = Set.difference vs t

minCut :: Graph -> (Weight, (Set Vert, Set Vert))
minCut = minimum . unfoldr (\g -> fmap (\(w, e, cs) -> ((w, cs), contractMetaEdge e g)) $ minCutPhase g) . makeMetaGraph
