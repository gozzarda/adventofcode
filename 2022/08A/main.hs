module Main where

import Data.Char (digitToInt)
import Data.List (transpose)

type Case = [[Int]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map (map digitToInt) . lines

showSoln :: Soln -> String
showSoln = unlines . return . show

solve :: Case -> Soln
solve gridHeights = length $ filter id $ concat gridVisibilities
  where
    rotRs = [id, map reverse . transpose, map reverse . reverse, transpose . map reverse]
    rotLs = id : reverse (tail rotRs)
    mapRotR f x = zipWith (\t' t -> t' $ f $ t x) rotLs rotRs
    prefMaxs = scanl max minBound . init
    gridCardinalMaxs = mapRotR (map prefMaxs) gridHeights
    girdThresholds = foldl1 ((zipWith . zipWith) min) gridCardinalMaxs
    gridVisibilities = (zipWith . zipWith) (>) gridHeights girdThresholds
