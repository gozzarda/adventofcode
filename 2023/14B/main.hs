module Main where

import Data.List (findIndices, partition, span, transpose)

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
solve = gridLoad . extrapolateCycle 1_000_000_000 step
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

pushEast :: [[Tile]] -> [[Tile]]
pushEast = map go
  where
    go [] = []
    go tiles = gaps ++ pile ++ wall ++ go next
      where
        (area, rest) = span (/= Block) tiles
        (pile, gaps) = partition (== Round) area
        (wall, next) = span (== Block) rest

step :: [[Tile]] -> [[Tile]]
step = pushEast . transpose . pushEast . transpose . pushWest . transpose . pushWest . transpose

extrapolateCycle :: Eq a => Int -> (a -> a) -> a -> a
extrapolateCycle n f x = ys !! n'
  where
    ys = iterate f x
    (_ : lwr : upr : _) = findIndices id $ zipWith (==) ys $ evens ys
    n' = if n <= upr then n else lwr + (n - lwr) `mod` (upr - lwr)

evens :: [a] -> [a]
evens (x : _ : xs) = x : evens xs
evens xs = xs
