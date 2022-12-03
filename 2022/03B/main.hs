module Main where

import Data.Char (ord)
import Data.List (intersect, splitAt)
import Data.List.Split (chunksOf)

type Case = [String]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = lines

showSoln :: Soln -> String
showSoln = unlines . return . show

solve :: Case -> Soln
solve = sum . map (itemPriority . head . foldl1 intersect) . chunksOf 3

itemPriority :: Char -> Int
itemPriority c | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
itemPriority c | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
