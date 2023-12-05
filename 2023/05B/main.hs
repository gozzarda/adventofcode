module Main where

import Data.Bifunctor (bimap)
import Data.List (stripPrefix)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)

type Prob = ([(Int, Int)], [[(Int, Int, Int)]])

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb s = (seeds, rangess)
  where
    ls = lines s
    (seedsls : rangessls) = splitMatches [""] ls
    seeds = pairs $ map read $ tail $ words $ head seedsls
    rangess = map (map readRange . tail) rangessls
    readRange l = let (dst : src : len : _) = map read $ words l in (dst, src, len)

pairs :: [a] -> [(a, a)]
pairs (l : r : xs) = (l, r) : (pairs xs)
pairs _ = []

splitMatch :: (Eq a) => [a] -> [a] -> ([a], [a])
splitMatch d = go []
  where
    go pref [] = (reverse pref, [])
    go pref t@(x : xs) = case stripPrefix d t of
      Just suff -> (reverse pref, suff)
      Nothing -> go (x : pref) xs

splitMatches :: (Eq a) => [a] -> [a] -> [[a]]
splitMatches d xs = case splitMatch d xs of
  (pref, []) -> [pref]
  (pref, suff) -> pref : splitMatches d suff

type Seg = (Int, Int)

type MultiSeg = [(Int, Int)]

segInterDiff :: (Int, Int) -> (Int, Int) -> (Maybe Seg, MultiSeg)
segInterDiff (ll, lu) (rl, ru)
  | lu < rl || ru < ll = (Nothing, [(ll, lu)])
  | otherwise = (Just (il, iu), dlus)
  where
    (il, iu) = (max ll rl, min lu ru)
    dlus = filter (\(l, u) -> l < u) [(ll, il), (iu, lu)]

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

shiftSegRem :: (Int, Int, Int) -> (Int, Int) -> (Maybe (Int, Int), MultiSeg)
shiftSegRem (d, s, l) seg = (fmap (both (+ delta)) mi, ds)
  where
    (mi, ds) = segInterDiff seg (s, s + l)
    delta = d - s

consMaybe :: Maybe a -> [a] -> [a]
consMaybe Nothing xs = xs
consMaybe (Just x) xs = x : xs

-- (those parts shifted by (d, s, l), parts left unshifted)
msShiftRem :: (Int, Int, Int) -> MultiSeg -> (MultiSeg, MultiSeg)
msShiftRem shift segs = bimap catMaybes concat $ unzip $ map (shiftSegRem shift) segs

msShiftsRem :: [(Int, Int, Int)] -> MultiSeg -> (MultiSeg, MultiSeg)
msShiftsRem shifts segs = go [] segs shifts
  where
    go movs rems [] = (movs, rems)
    go movs rems (s : ss) = go (smovs ++ movs) srems ss
      where
        (smovs, srems) = msShiftRem s rems

msMinimum :: MultiSeg -> Int
msMinimum = minimum . map fst

solve :: Prob -> Soln
solve (seeds, rangess) = minimum $ map (msMinimum . transform) segss
  where
    segss = map (\(lwr, len) -> [(lwr, lwr + len)]) seeds
    transform = foldl (flip (.)) id transforms
    transforms = map buildTransform rangess
    buildTransform ranges = uncurry (++) . msShiftsRem ranges
