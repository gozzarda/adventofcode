module Main where

import Data.Char (digitToInt)

type Prob = [[Int]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map (map digitToInt) . lines

solve :: Prob -> Soln
solve = sum . map solveBank

solveBank :: [Int] -> Int
solveBank joltages = maximum options
  where
    prefMaxs = scanl1 max joltages
    options = zipWith appendDigit prefMaxs $ tail joltages
    appendDigit msd lsd = msd * 10 + lsd
