module Main where

import Data.List.Split (splitOn)

type Range = (Int, Int)

type Case = [(Range, Range)]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readPair . lines

readPair :: String -> (Range, Range)
readPair s = let [l, r] = splitOn "," s in (readRange l, readRange r)

readRange :: String -> Range
readRange s = let [l, u] = splitOn "-" s in (read l, read u)

showSoln :: Soln -> String
showSoln = unlines . return . show

solve :: Case -> Soln
solve = length . filter (uncurry rangesIntersect)

rangesIntersect :: Range -> Range -> Bool
rangesIntersect (ll, lu) (rl, ru) = rl <= lu && ll <= ru
