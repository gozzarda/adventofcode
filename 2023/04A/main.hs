module Main where

import Data.Bifunctor (bimap)
import Data.List (intersect, stripPrefix)

type Card = ([Int], [Int])

type Prob = [Card]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map readCard . lines

readCard :: String -> Card
readCard = bimap (map read) (map read) . splitMatch ["|"] . drop 2 . words

splitMatch :: (Eq a) => [a] -> [a] -> ([a], [a])
splitMatch d = go []
  where
    go pref [] = (reverse pref, [])
    go pref t@(x : xs) = case stripPrefix d t of
      Just suff -> (reverse pref, suff)
      Nothing -> go (x : pref) xs

splitMatches :: (Eq a) => [a] -> [a] -> [[a]]
splitMatches d xs = case splitMatch d xs of
  (pref, []) -> [pref]
  (pref, suff) -> pref : splitMatches d suff

solve :: Prob -> Soln
solve = sum . map cardScore

cardScore :: Card -> Int
cardScore = score . length . uncurry intersect
  where
    score n = if n > 0 then 2 ^ (n - 1) else 0
