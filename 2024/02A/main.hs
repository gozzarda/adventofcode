module Main where

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
solve = length . filter isSafe
  where
    isSafe xs = all (inRange 1 3) ds || all (inRange (-3) (-1)) ds
      where
        ds = zipWith subtract xs (tail xs)
        inRange l u x = l <= x && x <= u
