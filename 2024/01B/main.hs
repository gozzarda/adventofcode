module Main where

import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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
solve lrs = sum $ map (\l -> l * Map.findWithDefault 0 l rcm) ls
  where
    (ls, rs) = unzip lrs
    rcm = Map.fromListWith (+) $ map (,1) rs
