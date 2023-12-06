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
solveRace t d = upr - lwr
  where
    score n = n * (t - n)
    peakn = t `div` 2
    lwr = binSearch ((> d) . score) 0 (t `div` 2 + 1)
    upr = binSearch ((<= d) . score) (t `div` 2) t

binSearch :: (Int -> Bool) -> Int -> Int -> Int
binSearch f = go
  where
    go l u | u - l == 1 = l
    go l u =
      let m = (l + u) `div` 2
       in if f m then go l m else go m u
