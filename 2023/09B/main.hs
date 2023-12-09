module Main where

import Data.List (unfoldr)

type Prob = [[Int]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map (map read . words) . lines

solve :: Prob -> Soln
solve = sum . map polyNextTerm

polyNextTerm :: [Int] -> Int
polyNextTerm = head . diffsPrevTerms . polyDiffs

polyDiffs :: [Int] -> [[Int]]
polyDiffs = takeWhile (not . null) . iterate diffs
  where
    diffs ys = zipWith subtract ys $ tail ys

diffsPrevTerms :: [[Int]] -> [Int]
diffsPrevTerms [(d : _)] = [d]
diffsPrevTerms ((d : _) : dss) = (d - t) : ts
  where
    ts@(t : _) = diffsPrevTerms dss
