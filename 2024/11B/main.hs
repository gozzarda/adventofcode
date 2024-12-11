module Main where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

type Prob = [Int]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map read . words

solve :: Prob -> Soln
solve = sum . map (go 75)
  where
    go 0 _ = 1
    go n 0 = go' (pred n) 1
    go n x
      | even (numDigits x) =
          let (l, r) = splitDigits x
           in go' (pred n) l + go' (pred n) r
    go n x = go' (pred n) (2024 * x)
    go' n x = Map.findWithDefault (go n x) (n, x) dpt
    dpt = Map.map (uncurry go) $ Map.fromList iis
    iis = [((n, x), (n, x)) | n <- [1 .. 75], x <- [1 .. 999]]

numDigits :: Int -> Int
numDigits = length . takeWhile (/= 0) . iterate (`div` 10)

splitDigits :: Int -> (Int, Int)
splitDigits x = let d = numDigits x `div` 2 in divMod x (10 ^ d)
