module Main where

import Data.Bits (shift, xor, (.&.))

type Prob = [Int]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map read . lines

solve :: Prob -> Soln
solve seeds = sum $ map (!! 2000) secretss
  where
    secretss = map (iterate step) seeds
    step = secretop 11 . secretop (-5) . secretop 6
    secretop n x = x `xor` shift x n .&. 0xFFFFFF
