module Main where

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
solve = sum . map (go 25)
  where
    go 0 _ = 1
    go n 0 = go (pred n) 1
    go n x
      | even (numDigits x) =
          let (l, r) = splitDigits x
           in go (pred n) l + go (pred n) r
    go n x = go (pred n) (2024 * x)

numDigits :: Int -> Int
numDigits = length . takeWhile (/= 0) . iterate (`div` 10)

splitDigits :: Int -> (Int, Int)
splitDigits x = let d = numDigits x `div` 2 in divMod x (10 ^ d)
