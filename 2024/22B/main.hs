module Main where

import Data.Array.Unboxed
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
solve seeds = maximum $ elems kva
  where
    base = 19
    digits = 4
    bound = base ^ digits
    kva = accumArray (+) 0 (0, bound) $ concatMap solve' seeds :: UArray Int Int
    solve' :: Int -> [(Int, Int)]
    solve' seed = filter ((/= 0) . snd) (assocs kva)
      where
        secrets = iterate step seed
        step = secretop 11 . secretop (-5) . secretop 6
        secretop n x = x `xor` shift x n .&. 0xFFFFFF
        prices = map (`mod` 10) $ reverse $ take 2001 secrets
        deltas = zipWith (-) prices $ drop 1 prices
        keys = drop digits (scanl keyf 0 deltas)
        keyf k d = (base * k + d + 9) `mod` bound
        kva :: UArray Int Int
        kva = accumArray (const id) 0 (0, bound) (zip keys prices)
