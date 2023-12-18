module Main where

import Data.Char (isHexDigit)
import Numeric (readHex)

data Dir = U | D | L | R deriving (Ord, Eq, Show, Read)

type Move = (Dir, Int)

type Prob = [Move]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map readMove . lines

readMove :: String -> Move
readMove s = (d, n)
  where
    (_ : _ : cw : _) = words s
    (nw, dw) = splitAt 5 $ filter isHexDigit cw
    d = case head dw of
      '0' -> R
      '1' -> D
      '2' -> L
      '3' -> U
    n = fst $ head $ readHex nw

solve :: Prob -> Soln
solve ms = area + perimeter `div` 2 + 1
  where
    (_, ns) = unzip ms
    perimeter = sum ns
    area = (abs $ polyTwiceArea $ movePoly ms) `div` 2

-- (x, y)
type Vect = (Int, Int)

(.+.) :: Vect -> Vect -> Vect
(.+.) (lx, ly) (rx, ry) = (lx + rx, ly + ry)

(.-.) :: Vect -> Vect -> Vect
(.-.) (lx, ly) (rx, ry) = (lx - rx, ly - ry)

(*.) :: Int -> Vect -> Vect
(*.) l (rx, ry) = (l * rx, l * ry)

areaProd :: Vect -> Vect -> Int
areaProd (lx, ly) (rx, ry) = lx * ry - rx * ly

dirVect :: Dir -> Vect
dirVect U = (0, 1)
dirVect D = (0, -1)
dirVect L = (-1, 0)
dirVect R = (1, 0)

moveVect :: Move -> Vect
moveVect (d, n) = n *. (dirVect d)

type Poly = [Vect]

movePoly :: [Move] -> Poly
movePoly = scanl (.+.) (0, 0) . map moveVect

polyTwiceArea :: Poly -> Int
polyTwiceArea (p : ps) = sum $ zipWith areaProd (p : ps) (ps ++ [p])
