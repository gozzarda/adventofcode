module Main where

import Data.Maybe (fromJust)

type Prob = (Int, Int)

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb s = (t, d)
  where
    (tl : dl : _) = lines s
    t = read $ concat $ tail $ words tl
    d = read $ concat $ tail $ words dl

solve :: Prob -> Soln
solve = uncurry solveRace

-- Counts n * (t - n) > d
-- Solves t * n - n ^ 2 = d + 1
-- n ^ 2 - t * n + d + 1 = 0
-- delta = sqrt(t ^ 2 - 4 * (d + 1))
-- lwr = ceil((t - delta) / 2)
-- upr = floor((t + delta) / 2)
-- count = upr - lwr + 1
solveRace :: Int -> Int -> Int
solveRace t d = (t + delta) `div` 2 + (delta - t) `div` 2 + 1
  where
    delta2 = t ^ 2 - 4 * (d + 1)
    delta = expSearch (\x -> delta2 < x * x)

-- Find the first x >= 0 that satisfies step function
expSearch :: (Int -> Bool) -> Int
expSearch f = go 0 1
  where
    go l u | not $ f u = go u (u + u)
    go l u | u - l == 1 = l
    go l u =
      let m = (l + u) `div` 2
       in if f m then go l m else go m u
