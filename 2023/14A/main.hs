module Main where

import Data.List (partition, span, transpose)

data Tile = Block | Round | Empty deriving (Ord, Eq)

type Prob = [[Tile]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = (map . map) readTile . lines

readTile :: Char -> Tile
readTile '#' = Block
readTile 'O' = Round
readTile '.' = Empty

solve :: Prob -> Soln
solve = gridLoad . transpose . pushWest . transpose

gridLoad :: [[Tile]] -> Int
gridLoad = sum . zipWith (*) [1 ..] . reverse . map (length . filter (== Round))

pushWest :: [[Tile]] -> [[Tile]]
pushWest = map go
  where
    go [] = []
    go tiles = pile ++ gaps ++ wall ++ go next
      where
        (area, rest) = span (/= Block) tiles
        (pile, gaps) = partition (== Round) area
        (wall, next) = span (== Block) rest
