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
solveBank joltages = go base joltages !! 12
  where
    base = map (const 0) joltages
    go prefMaxs [] = [maximum prefMaxs]
    go prefMaxs js@(_ : js') = maximum prefMaxs : go prefMaxs' js'
      where
        options = zipWith appendInt prefMaxs js
        appendInt msd lsd = msd * 10 + lsd
        prefMaxs' = scanl1 max options
