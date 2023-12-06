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
    delta = bs 0 1
    bs l u | u * u <= delta2 = bs u (u + u)
    bs l u | l + 1 == u = l
    bs l u =
      let m = (l + u) `div` 2
       in if delta2 < (m * m) then bs l m else bs m u
