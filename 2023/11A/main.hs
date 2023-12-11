module Main where

import Data.Bool (bool)
import Data.List (transpose)

type Prob = [[Bool]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map (map (== '#')) . lines

solve :: Prob -> Soln
solve bss = solveRows bss + solveRows (transpose bss)
  where
    solveRows = histPairDistSum . rowCounts

rowCounts :: [[Bool]] -> [Int]
rowCounts = map (length . filter id)

expansionFactor :: Int
expansionFactor = 2

histPairDistSum :: [Int] -> Int
histPairDistSum ns = total
  where
    step (total, delta, opens) 0 = (total, delta + expansionFactor * opens, opens)
    step (total, delta, opens) n = let delta' = delta + opens in (total + n * delta', delta', opens + n)
    (total, _, _) = foldl step (0, 0, 0) ns
