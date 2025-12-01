module Main where

data Dir = L | R deriving (Ord, Eq, Show)

type Rot = (Dir, Int)

type Prob = [Rot]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map readRot . lines

readRot :: String -> Rot
readRot (c:cs) = let n = read cs in
  case c of
    'L' -> (L, n)
    'R' -> (R, n)

solve :: Prob -> Soln
solve rs = sum $ zipWith countZeros ps rs
  where
    ps = scanl dialTurn 50 rs

dialSize :: Int
dialSize = 100

dialTurn :: Int -> Rot -> Int
dialTurn pos (d, n) = case d of
  L -> (pos - n) `mod` dialSize
  R -> (pos + n) `mod` dialSize

countZeros :: Int -> Rot -> Int
countZeros pos (d, n) = (n + offset) `div` dialSize
  where
    offset = case d of
      L -> (-pos) `mod` dialSize
      R -> pos
