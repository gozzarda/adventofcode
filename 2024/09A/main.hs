module Main where

import Data.Char (digitToInt, isDigit)
import Data.Maybe

type Prob = [Int]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map digitToInt . takeWhile isDigit

solve :: Prob -> Soln
solve ws = sum $ zipWith (*) [0 ..] is'
  where
    is = concatMap (uncurry replicate) $ zip ws $ interleave (map Just [0 ..]) (repeat Nothing)
    is' = zipWith const (go is $ reverse $ catMaybes is) (catMaybes is)
      where
        go (i : is) (r : rs) = case i of
          Nothing -> r : go is rs
          Just i' -> i' : go is (r : rs)

interleave :: [a] -> [a] -> [a]
interleave xs [] = xs
interleave [] ys = ys
interleave (x : xs) (y : ys) = x : y : interleave xs ys
