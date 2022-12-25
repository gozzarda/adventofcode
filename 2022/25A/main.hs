module Main where

import Data.List (dropWhileEnd, foldl')

type Digit = Char

type SNAFU = [Digit]

type Case = [SNAFU]

type Soln = SNAFU

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map reverse . lines

showSoln :: Soln -> String
showSoln = unlines . return . reverse

snafuHalfAdder :: Digit -> Digit -> (Digit, Digit)
snafuHalfAdder l '0' = ('0', l)
snafuHalfAdder '0' r = ('0', r)
snafuHalfAdder '-' '1' = ('0', '0')
snafuHalfAdder '=' '2' = ('0', '0')
snafuHalfAdder '=' '=' = ('-', '1')
snafuHalfAdder '=' '-' = ('-', '2')
snafuHalfAdder '-' '-' = ('0', '=')
snafuHalfAdder '=' '1' = ('0', '-')
snafuHalfAdder '-' '2' = ('0', '1')
snafuHalfAdder '1' '1' = ('0', '2')
snafuHalfAdder '1' '2' = ('1', '=')
snafuHalfAdder '2' '2' = ('1', '-')
snafuHalfAdder l r = snafuHalfAdder r l

snafuFullAdder :: Digit -> Digit -> Digit -> (Digit, Digit)
snafuFullAdder c l r = (c', s')
  where
    (clr, s) = snafuHalfAdder l r
    (cs, s') = snafuHalfAdder c s
    (cc, c') = snafuHalfAdder clr cs

snafuAdd :: SNAFU -> SNAFU -> SNAFU
snafuAdd l r = dropWhileEnd (== '0') $ snafuAdd' '0' l r
  where
    snafuAdd' '0' [] [] = []
    snafuAdd' c [] [] = [c]
    snafuAdd' c [] rs = snafuAdd' c ['0'] rs
    snafuAdd' c ls [] = snafuAdd' c ls ['0']
    snafuAdd' c (l : ls) (r : rs) = let (c', s) = snafuFullAdder c l r in s : snafuAdd' c' ls rs

snafuSum :: [SNAFU] -> SNAFU
snafuSum = foldl' snafuAdd []

solve :: Case -> Soln
solve = snafuSum
