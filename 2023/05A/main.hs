module Main where

import Data.List (stripPrefix)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)

type Prob = ([Int], [[(Int, Int, Int)]])

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
    seeds = map read $ tail $ words $ head seedsls
    rangess = map (map readRange . tail) rangessls
    readRange l = let (dst : src : len : _) = map read $ words l in (dst, src, len)

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

solve :: Prob -> Soln
solve (seeds, rangess) = minimum $ map transform seeds
  where
    transform = foldl (flip (.)) id transforms
    transforms = map buildTransform rangess
    buildTransform ranges i = fromMaybe i $ firstJust $ map (flip applyRange i) ranges
    applyRange (d, s, l) i = if s <= i && i < s + l then Just (i - s + d) else Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes
