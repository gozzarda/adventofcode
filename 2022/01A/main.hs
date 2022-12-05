module Main where

import Data.Function (on)
import Data.List (groupBy)

type Case = [[Int]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map (map read) . splitWhen null . lines

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f = filter (not . f . head) . groupBy ((==) `on` f)

showSoln :: Soln -> String
showSoln = unlines . return . show

solve :: Case -> Soln
solve = maximum . map sum
