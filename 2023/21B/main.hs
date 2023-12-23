module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
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

numSteps :: Int
numSteps = 26_501_365

solve :: Prob -> Soln
solve tss = (extPoly 2 phase) !! cycnum
  where
    ctx@(nr, nc, itm) = makeContext tss
    srcs = Map.keysSet $ Map.filter (== 'S') itm
    cyclen = if nr == nc then nr else error "pick a safe cyclen based on input"
    (cycnum, offset) = divMod numSteps cyclen
    diffs = map length $ bfsLayers (neighbours ctx) srcs
    steps = zipWith (+) diffs $ 0 : 0 : steps
    phase = every cyclen $ drop offset steps

every :: Int -> [a] -> [a]
every n xs@(x : _) = x : (every n $ drop n xs)
every _ [] = []

extPoly :: Int -> [Int] -> [Int]
extPoly (-1) xs = takeWhile (/= 0) xs ++ repeat 0
extPoly n xs@(x : xt) = xs'
  where
    ds = zipWith subtract xs xt
    ds' = extPoly (n - 1) ds
    xs' = x : zipWith (+) xs' ds'

type Coord = (Int, Int)

(.+.) :: Coord -> Coord -> Coord
(.+.) (ll, lr) (rl, rr) = (ll + rl, lr + rr)

type Context = (Int, Int, Map Coord Char)

makeContext :: [[Char]] -> Context
makeContext tss = (nr, nc, itm)
  where
    nr = length tss
    nc = maximum $ map length tss
    itm = Map.fromList $ concat $ zipWith zip [[(r, c) | c <- [0 ..]] | r <- [0 ..]] tss

type Vert = Coord

neighbours :: Context -> Vert -> [Vert]
neighbours (nr, nc, itm) = go
  where
    deltas = [(-1, 0), (0, -1), (0, 1), (1, 0)]
    go u = filter isValid $ map (u .+.) deltas
    isValid (vr, vc) = Set.member (vr `mod` nr, vc `mod` nc) valids
    valids = Map.keysSet $ Map.filter (/= '#') itm

bfsLayers :: (Vert -> [Vert]) -> Set Vert -> [Set Vert]
bfsLayers adjf = go Set.empty
  where
    go _ currs | Set.null currs = []
    go seen currs = currs : go seen' nexts
      where
        seen' = Set.union seen currs
        nexts = Set.difference (Set.fromList $ concatMap adjf currs) seen'
