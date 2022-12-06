module Main where

import Data.List (findIndex, nub, tails)
import Data.Maybe (fromJust)

type Case = String

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSolns . map solve . readCases

readCases :: String -> [Case]
readCases = lines

showSolns :: [Soln] -> String
showSolns = unlines . map show

solve :: Case -> Soln
solve = (+ header_length) . fromJust . findIndex ((== header_length) . length . nub) . map (take header_length) . tails
  where
    header_length = 4
