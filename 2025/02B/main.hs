module Main where

import Data.List (uncons, (\\))
import Debug.Trace

type Prob = [(Int, Int)]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map readRange . splitBy (== ',')

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p xs = case break p $ dropWhile p xs of
  ([], _) -> []
  (pref, suff) -> pref : splitBy p suff

readRange :: String -> (Int, Int)
readRange cs = (read lhs, read rhs)
  where
    (lhs, (_ : rhs)) = span (/= '-') cs

solve :: Prob -> Soln
solve = sum . concatMap findInvalids

findInvalids :: (Int, Int) -> [Int]
findInvalids (lwr, upr) = takeWhile (<= upr) $ nextInvalids lwr

nextInvalids :: Int -> [Int]
nextInvalids n = let n' = nextInvalid n in n' : (nextInvalids $ succ n')

nextInvalid :: Int -> Int
nextInvalid n = if null ns then nextInvalid (10 ^ nd) else minimum ns
  where
    nd = numDigitsDec n
    bds = init $ factors nd -- all block sizes (excluding one block)
    bs = map (10 ^) bds -- all bases
    ns = map (\b -> nextInvalidBase b n) bs

numDigitsDec :: Int -> Int
numDigitsDec 0 = 0
numDigitsDec n = 1 + numDigitsDec (n `div` 10)

factors :: Int -> [Int]
factors n = smalls ++ dropWhile (\x -> x * x == n) larges
  where
    smalls = filter (\x -> n `mod` x == 0) $ takeWhile (\x -> x * x <= n) [1 ..]
    larges = map (n `div`) $ reverse smalls

-- nextInvalid assuming the number is in base b (so base 1000 is blocks of 3 digits)
nextInvalidBase :: Int -> Int -> Int
nextInvalidBase b n =
  if d == b
    then nextInvalid (fromDigits b ozs)
    else fromDigits b ds'
  where
    ds = toDigits b n
    (msd : lsds) = reverse ds
    lsds' = dropWhile (== msd) lsds
    d = case uncons lsds' of
      Nothing -> msd
      Just (firstDiffDigit, _) ->
        if msd >= firstDiffDigit
          then msd
          else succ msd
    ds' = map (const d) ds
    ozs = 1 : map (const 0) ds

toDigits :: Int -> Int -> [Int]
toDigits _ 0 = []
toDigits b n = let (n', d) = divMod n b in d : toDigits b n'

fromDigits :: Int -> [Int] -> Int
fromDigits b [] = 0
fromDigits b (d : ds) = d + b * (fromDigits b ds)
