{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative (liftA2, (<|>))
import Data.Char
import Data.List (delete, subsequences)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

type Case = [(String, Int, [String])]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map (fromJust . doReadP parseValve) . lines

doReadP :: (Show a) => ReadP a -> String -> Maybe a
doReadP p = fmap fst . listToMaybe . readP_to_S p

parse :: Read a => ReadP a
parse = ReadPrec.readPrec_to_P readPrec ReadPrec.minPrec

parseValve :: ReadP (String, Int, [String])
parseValve = do
  string "Valve "
  label <- munch isUpper
  string " has flow rate="
  flow <- parse
  string "; tunnels lead to valves " +++ string "; tunnel leads to valve "
  tunnels <- sepBy (munch isUpper) (string ", ")
  eof
  return (label, flow, tunnels)

showSoln :: Soln -> String
showSoln = unlines . return . show

data SublistTrie k a = SublistTrie [k] (BitsTrie a)

data BitsTrie a = BitsLeaf a | BitsFork (BitsTrie a) (BitsTrie a)

sublistTrie :: [k] -> a -> SublistTrie k a
sublistTrie ks = SublistTrie ks . go ks
  where
    go [] x = BitsLeaf x
    go (_ : ks) x = let t = go ks x in BitsFork t t

sublistTrieMapWithKey :: ([k] -> a -> b) -> SublistTrie k a -> SublistTrie k b
sublistTrieMapWithKey f (SublistTrie ks t) = SublistTrie ks $ go [] ks t
  where
    go stack _ (BitsLeaf x) = BitsLeaf $ f (reverse stack) x
    go stack (k : ks) (BitsFork lt rt) = BitsFork (go stack ks lt) (go (k : stack) ks rt)

sublistTrieFind :: (Eq k) => [k] -> SublistTrie k a -> a
sublistTrieFind ls (SublistTrie ks t) = go ks ls t
  where
    go [] [] (BitsLeaf x) = x
    go (_ : ks) [] (BitsFork lt rt) = go ks [] lt
    go (k : ks) (l : ls) (BitsFork lt rt) = if k == l then go ks ls rt else go ks (l : ls) lt

type DPK = (Int, String, [String])

type DPT a = Map (Int, String) (SublistTrie String a)

dptConst :: Int -> [String] -> a -> DPT a
dptConst n ks x = Map.fromList [((t, k), sublistTrie ks x) | t <- [1 .. n], k <- ks]

dptMapWithKey :: (DPK -> a -> b) -> DPT a -> DPT b
dptMapWithKey f = Map.mapWithKey (sublistTrieMapWithKey . (\(t, k) ks -> f (t, k, ks)))

dptMapKey :: (DPK -> a) -> DPT b -> DPT a
dptMapKey f = dptMapWithKey (const . f)

dptLookup :: DPK -> DPT a -> Maybe a
dptLookup (t, k, ks) = fmap (sublistTrieFind ks) . Map.lookup (t, k)

type AdjMap = Map String (Map String Int)

dp :: Map String Int -> AdjMap -> Int -> DPT Int
dp flowm adjm n = dpt
  where
    ks = Map.keys adjm
    dpt = dptMapKey dpf $ dptConst n ks ()
    dpf (t, k, ks) = (flowm ! k) * t + maximum opts
      where
        opts = 0 : mapMaybe f ks
        f v = Map.lookup v (adjm ! k) >>= (\d -> dptLookup (t - d - 1, v, delete v ks) dpt)

floydwarshall :: [[Maybe Int]] -> [[Maybe Int]]
floydwarshall ds = let n = length ds in foldl step ds [0 .. n - 1]
  where
    minm l r = liftA2 min l r <|> l <|> r
    step ds k = (zipWith . zipWith) minm ds $ map (`map` kjs) iks
      where
        iks = map (liftA2 (+)) (ds !! k)
        kjs = map (!! k) ds

apsp :: AdjMap -> AdjMap
apsp adjm = Map.fromAscList $ zipWith (\k wm -> (k, Map.delete k wm)) ks wms'
  where
    (ks, wms) = unzip $ Map.assocs adjm
    ds = floydwarshall $ map (flip map ks . flip Map.lookup) wms
    wms' = map (Map.fromAscList . catMaybes . zipWith (fmap . (,)) ks) ds

solve :: Case -> Soln
solve kfas = maximum $ zipWith (+) subs (reverse subs)
  where
    t = 26
    src = "AA"
    (ks, fs, as) = unzip3 kfas
    flowm = Map.fromList $ zip ks fs
    flowm' = Map.filterWithKey (\k f -> f > 0 || k == src) flowm
    adjm = apsp $ Map.fromList $ zip ks $ map (Map.fromList . map (,1)) as
    adjm' = Map.map (`Map.intersection` flowm') $ Map.intersection adjm flowm'
    dpt = dp flowm' adjm' t
    ks' = delete src $ Map.keys adjm'
    subs = map (\ks -> fromJust $ dptLookup (t, src, ks) dpt) $ subsequences ks'
