module Main where

import Data.List.Split (splitWhen)

type Case = [[Int]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map (map read) . splitWhen null . lines

showSoln :: Soln -> String
showSoln = unlines . return . show

solve :: Case -> Soln
solve = maximum . map sum
