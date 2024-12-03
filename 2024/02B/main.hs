module Main where

type Prob = [[Int]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map (map read . words) . lines

solve :: Prob -> Soln
solve = length . filter isSafe
  where
    isSafe xs = isGradIncDamp xs || isGradDecDamp xs

isGradIncDamp :: [Int] -> Bool
isGradIncDamp = go 1 []
  where
    go _ _ [] = True
    go n [] (s : ss) = go n [s] ss
    go n (p : ps) (s : ss)
      | p < s && s - p <= 3 = go n (s : p : ps) ss
      | n > 0 = go (pred n) ps (s : ss) || go (pred n) (p : ps) ss
      | otherwise = False

isGradDecDamp :: [Int] -> Bool
isGradDecDamp = isGradIncDamp . reverse
