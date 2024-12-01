module Main where

import Data.List (sort)

type Prob = [(Int, Int)]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map ((\(l : r : _) -> (l, r)) . map read . words) . lines

solve :: Prob -> Soln
solve lrs = sum $ map abs $ zipWith (-) (sort ls) (sort rs)
  where
    (ls, rs) = unzip lrs
