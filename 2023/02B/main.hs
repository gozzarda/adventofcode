module Main where

import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)

type Hand = [(Int, String)]

type Game = (Int, [Hand])

type Prob = [Game]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map readGame . lines
  where
    readGame s = (game_id, hands)
      where
        (header, body) = splitMatch ": " s
        game_id = read $ snd $ splitMatch " " header
        hands = readHands body
    readHands = map readHand . splitMatches "; "
    readHand = map readCube . splitMatches ", "
    readCube = (\(pref, suff) -> (read pref, suff)) . splitMatch " "

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
solve = sum . map histPower . map gameToHist

handToHist :: Hand -> Map String Int
handToHist = Map.fromListWith (+) . map swap

histsMax :: [Map String Int] -> Map String Int
histsMax = Map.unionsWith max

gameToHist :: Game -> Map String Int
gameToHist = histsMax . map handToHist . snd

histPower :: Map String Int -> Int
histPower = Map.foldr' (*) 1
