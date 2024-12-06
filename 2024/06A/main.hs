module Main where

import Data.Array
import Data.List (group, sort)

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

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (lf, ls) (rf, rs) = (f lf rf, f ls rs)

solve :: Prob -> Soln
solve css = length $ group $ sort $ takeWhile (inRange rcBounds) coords
  where
    nr = length css
    nc = maximum $ map length css
    rcBounds = ((0, 0), (nr - 1, nc - 1))
    grid = listArray rcBounds $ concat css
    get rc = if inRange rcBounds rc then grid ! rc else '?'
    stepState (rc, drc@(dr, dc)) =
      let rc' = both2 (+) rc drc
       in if get rc' == '#' then (rc, (dc, -dr)) else (rc', drc)
    initCoord = fst $ head $ filter ((== '^') . snd) $ assocs grid
    initState = (initCoord, (-1, 0))
    states = iterate stepState initState
    coords = map fst states
