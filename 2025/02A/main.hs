module Main where

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
nextInvalid n =
  if rd /= 0
    then nextInvalid (10 ^ nd)
    else block * (10 ^ hd) + block
  where
    nd = decDigits n
    (hd, rd) = nd `divMod` 2
    (pref, suff) = n `divMod` (10 ^ hd)
    block = if pref >= suff then pref else succ pref

decDigits :: Int -> Int
decDigits 0 = 0
decDigits n = 1 + decDigits (n `div` 10)
