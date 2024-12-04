module Main where

import Data.Array
import Data.List (isPrefixOf)

type Prob = [[Char]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = lines

solve :: Prob -> Soln
solve css = length $ filter isXMAS $ indices grid
  where
    nr = length css
    nc = maximum $ map length css
    rcbounds = ((0, 0), (nr - 1, nc - 1))
    grid = listArray rcbounds $ concat css
    get rc = if inRange rcbounds rc then grid ! rc else '.'
    getFore (r, c) = [get (r - 1, c - 1), get (r, c), get (r + 1, c + 1)]
    getBack (r, c) = [get (r - 1, c + 1), get (r, c), get (r + 1, c - 1)]
    isForeMAS rc = let w = getFore rc in w == "MAS" || w == "SAM"
    isBackMAS rc = let w = getBack rc in w == "MAS" || w == "SAM"
    isXMAS rc = isForeMAS rc && isBackMAS rc
