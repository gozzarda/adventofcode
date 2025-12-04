module Main where

import Data.Array.Unboxed (UArray, listArray, (!?))
import Data.List (elemIndices)

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

type Z2 = (Int, Int)

solve :: Prob -> Soln
solve xss = length $ filter ((< 4) . numAdj) rcs
  where
    xarr = gridArray xss
    rcs = elemIndices2D '@' xss
    numAdj rc = length $ filter (== Just '@') $ map (xarr !?) (neighbours rc)

gridArray :: [[Char]] -> UArray Z2 Char
gridArray xss = listArray ((0, 0), (pred nr, pred nc)) $ concatMap (take nc) xss
  where
    nr = length xss
    nc = case map length xss of
      [] -> 0
      ls -> minimum ls

elemIndices2D :: (Eq a) => a -> [[a]] -> [(Int, Int)]
elemIndices2D x = concat . zipWith (map . (,)) [0 ..] . map (elemIndices x)

deltas :: [Z2]
deltas = [(r, c) | r <- [-1, 0, 1], c <- [-1, 0, 1], r /= 0 || c /= 0]

neighbours :: Z2 -> [Z2]
neighbours = flip map deltas . addZ2

addZ2 :: Z2 -> Z2 -> Z2
addZ2 (lx, ly) (rx, ry) = (lx + rx, ly + ry)
