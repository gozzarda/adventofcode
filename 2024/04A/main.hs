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

needle :: String
needle = "XMAS"

dirs :: [(Int, Int)]
dirs = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (lf, ls) (rf, rs) = (f lf rf, f ls rs)

solve :: Prob -> Soln
solve css = length $ filter (isPrefixOf needle) opts
  where
    nr = length css
    nc = maximum $ map length css
    rcbounds = ((0, 0), (nr - 1, nc - 1))
    grid = listArray rcbounds $ concat css
    get rc = if inRange rcbounds rc then grid ! rc else '.'
    gets rc dir = map get $ iterate (both2 (+) dir) rc
    opts = [gets rc dir | rc <- indices grid, dir <- dirs]
