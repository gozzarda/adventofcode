module Main where

import Data.Char (digitToInt, isDigit)

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
solve = sum . map readCalibrationValue

readCalibrationValue :: String -> Int
readCalibrationValue = (\xs -> head xs * 10 + last xs) . map digitToInt . filter isDigit
