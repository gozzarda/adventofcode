module Main where

import Data.Array
import Data.List (group, sort)
import Debug.Trace

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

-- Slow-as naive
-- Alt: Find points where path crosses itself at the right angle?
-- Would need to reverse engineer paths leading into initState as well though :/
solve :: Prob -> Soln
solve css = length loopOrcs
  where
    nr = length css
    nc = maximum $ map length css
    rcBounds = ((0, 0), (nr - 1, nc - 1))
    grid = listArray rcBounds $ concat css
    get rc = if inRange rcBounds rc then grid ! rc else '?'
    get' orc rc = if orc == rc then '#' else get rc
    stepState g (rc, drc@(dr, dc)) =
      let rc' = both2 (+) rc drc
       in if g rc' == '#' then (rc, (dc, -dr)) else (rc', drc)
    initCoord = fst $ head $ filter ((== '^') . snd) $ assocs grid
    initState = (initCoord, (-1, 0))
    states = iterate (stepState get) initState
    coords = takeWhile (inRange rcBounds) $ map fst states
    willLoop orc = traceShow orc $ length (group $ sort $ take n states) /= n
      where
        states = iterate (stepState $ get' orc) initState
        n = 4 * rangeSize rcBounds + 1
    loopOrcs = filter willLoop $ filter (/= initCoord) coords
