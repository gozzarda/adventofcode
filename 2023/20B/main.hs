module Main where

import Data.List (foldl1)

-- Could solve this by decomposing the graph into SCCs, toposorting the
-- resulting DAG, and analysing the cyclic behaviour of each component.
-- But it is easier to just manually inspect the input and notice that it's just
-- four binary counters in parallel.

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = const (showSoln solve)

showSoln :: Soln -> String
showSoln = unlines . return . show

solve :: Soln
solve = foldl1 lcm [0b111110100011, 0b111101001101, 0b111101001111, 0b111110111011]
