module Main where

import Data.List (elemIndex, inits, stripPrefix, tails, transpose)

import Debug.Trace

type Prob = [[[Char]]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = splitMatches [""] . lines

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
solve = sum . map gridValue
  where
    gridValue xss = 100 * (reflectionIndex xss) + (reflectionIndex $ transpose xss)
    reflectionIndex xss = maybe 0 id $ elemIndex 1 $ zipWith diffCount (tails xss) (map reverse $ inits xss)
    diffCount lss rss = length $ filter id $ concat $ zipWith (zipWith (/=)) lss rss
