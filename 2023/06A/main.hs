module Main where

import Data.Maybe (fromJust)

type Prob = [(Int, Int)]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb s = zip ts ds
  where
    (tl : dl : _) = lines s
    ts = map read $ tail $ words tl
    ds = map read $ tail $ words dl

solve :: Prob -> Soln
solve = product . map (uncurry solveRace)

solveRace :: Int -> Int -> Int
solveRace t d = (t + delta) `div` 2 + (delta - t) `div` 2 + 1
  where
    delta2 = t ^ 2 - 4 * (d + 1)
    delta = bs 0 delta2
    bs l u | l == u = l
    bs l u =
      let m = (l + u + 1) `div` 2
       in if delta2 < (m * m) then bs l (m - 1) else bs m u
