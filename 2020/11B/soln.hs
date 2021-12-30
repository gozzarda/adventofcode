import           Data.Bool       (bool)
import           Data.List       (transpose)
import           Data.Maybe      (catMaybes)

import           Control.DeepSeq (deepseq)
import           Debug.Trace

type Case = [[Char]]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = lines

display :: Soln -> String
display = unlines . return . show

skewL :: a -> [[a]] -> [[a]]
skewL v grid = zipWith3 (\pl vs pr -> pl ++ vs ++ pr) pads grid (reverse pads)
  where
    pads = take (length grid) $ map (flip replicate v) [0..]

unskewL :: [[a]] -> [[a]]
unskewL grid = map (take width) aligned
  where
    aligned = zipWith drop [0..] grid
    width = minimum $ map length aligned

skewR :: a -> [[a]] -> [[a]]
skewR v = (map reverse) . (skewL v) . (map reverse)

unskewR :: [[a]] -> [[a]]
unskewR = (map reverse) . unskewL . (map reverse)

visStep :: Char -> Char -> Char
visStep l r = if r `elem` "#L" then r else l

lVis :: [[Char]] -> [[Char]]
lVis = map (init . scanl (visStep) '_')

rVis :: [[Char]] -> [[Char]]
rVis = map (tail . scanr (flip visStep) '_')

uVis :: [[Char]] -> [[Char]]
uVis = transpose . lVis . transpose

dVis :: [[Char]] -> [[Char]]
dVis = transpose . rVis . transpose

ulVis :: [[Char]] -> [[Char]]
ulVis = unskewR . uVis . (skewR '_')

urVis :: [[Char]] -> [[Char]]
urVis = unskewL . uVis . (skewL '_')

dlVis :: [[Char]] -> [[Char]]
dlVis = unskewL . dVis . (skewL '_')

drVis :: [[Char]] -> [[Char]]
drVis = unskewR . dVis . (skewR '_')

visible :: [[Char]] -> [[Int]]
visible grid = vss
  where
    csss = map ($ grid) [lVis, rVis, uVis, dVis, ulVis, urVis, dlVis, drVis]
    vsss = map (map (map $ bool 0 1 . (=='#'))) csss
    vss = foldl1 (zipWith (zipWith (+))) vsss

step :: [[Char]] -> [[Char]]
step grid = zipWith (zipWith update) grid vis
  where
    vis = visible grid
    update '.' _ = '.'
    update 'L' n = if n == 0 then '#' else 'L'
    update '#' n = if n >= 5 then 'L' else '#'

solve :: Case -> Soln
solve grid = taken
  where
    steps = iterate (\g -> let g' = step g in trace (unlines g') g') grid
    -- steps = iterate step grid
    final = fst $ head $ filter (uncurry (==)) $ zip steps $ tail steps
    taken = length $ filter (=='#') $ concat final
