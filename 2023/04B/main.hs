module Main where

import Data.Bifunctor (bimap, first, second)
import Data.List (intersect, splitAt, stripPrefix)

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
solve cards = go $ zip (map cardScore cards) (repeat 1)
  where
    cardScore = length . uncurry intersect
    go [] = 0
    go ((score, count) : cards) = count + go (prefMap (second (+ count)) score cards)

prefMap :: Show a => (a -> a) -> Int -> [a] -> [a]
prefMap f n = uncurry (++) . first (map f) . splitAt n
