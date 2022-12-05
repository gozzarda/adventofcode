module Main where

import Data.Char (isAsciiLower, isAsciiUpper, ord)
import Data.List (intersect, splitAt)

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
solve = sum . map (itemPriority . head . uncurry intersect . bisectList)

itemPriority :: Char -> Int
itemPriority c | isAsciiLower c = ord c - ord 'a' + 1
itemPriority c | isAsciiUpper c = ord c - ord 'A' + 27

bisectList :: [a] -> ([a], [a])
bisectList xs = let n = div (length xs) 2 in splitAt n xs
