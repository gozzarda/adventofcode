module Main where

import Data.Bits
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

type BitSet = Integer

maximumClique :: Set Vert -> Map Vert (Set Vert) -> Set Vert
maximumClique vs adjm = fromBitSet vs $ snd $ dfs (Set.empty, 0) (0, vs)
  where
    adjs u = Map.findWithDefault Set.empty u adjm
    dfs :: (Set BitSet, BitSet) -> (BitSet, Set Vert) -> (Set BitSet, BitSet)
    dfs (seen, best) (us, _) | Set.member us seen = (seen, best)
    dfs (seen, best) (us, ns) = foldl dfs sb uvs
      where
        sb = (Set.insert us seen, maxOn popCount best us)
        uvs = map uvf (Set.elems ns)
        uvf v = (bsInsert vs v us, Set.intersection ns (adjs v))

bsInsert :: (Ord a) => Set a -> a -> BitSet -> BitSet
bsInsert xs x b = b .|. (shiftL 1 $ Set.findIndex x xs)

fromBitSet :: (Ord a) => Set a -> BitSet -> Set a
fromBitSet xs b = Set.fromList $ map (`Set.elemAt` xs) is
  where
    is = go 0 b
    go _ 0 = []
    go i b = (if testBit b 0 then (i:) else id) (go (succ i) (shiftR b 1))

maxOn :: (Ord b) => (a -> b) -> a -> a -> a
maxOn f x y = if f x <= f y then y else x
