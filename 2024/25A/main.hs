module Main where

import Data.List (isPrefixOf, partition, transpose)

type Prob = [[[Char]]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = splitBy null . lines

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p xs = case break p $ dropWhile p xs of
  ([], _) -> []
  (pref, suff) -> pref : splitBy p suff

solve :: Prob -> Soln
solve schematics = length [() | l <- ls', k <- ks', fit l k]
  where
    h = maximum $ map length schematics
    (ls, ks) = partition (all (isPrefixOf "#")) (map transpose schematics)
    ls' = (map . map) (length . takeWhile (== '#')) ls
    ks' = (map . map) (length . takeWhile (/= '#')) ks
    fit l k = and $ zipWith (<=) l k
