module Main where

import Data.Bool (bool)
import Data.Char (digitToInt, isDigit)
import Data.Foldable (find)
import Data.Function ((&))
import Data.List (isPrefixOf, tails)
import Data.Maybe (catMaybes, listToMaybe)

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
readCalibrationValue = (\xs -> head xs * 10 + last xs) . readDigits

readDigits :: String -> [Int]
readDigits = catMaybes . map readDigit . tails

readDigit :: String -> Maybe Int
readDigit = listToMaybe . catMaybes . flip map rules . (&)
  where
    rules =
      [ (fmap digitToInt . find isDigit . listToMaybe)
      , (bool Nothing (Just 1) . isPrefixOf "one")
      , (bool Nothing (Just 2) . isPrefixOf "two")
      , (bool Nothing (Just 3) . isPrefixOf "three")
      , (bool Nothing (Just 4) . isPrefixOf "four")
      , (bool Nothing (Just 5) . isPrefixOf "five")
      , (bool Nothing (Just 6) . isPrefixOf "six")
      , (bool Nothing (Just 7) . isPrefixOf "seven")
      , (bool Nothing (Just 8) . isPrefixOf "eight")
      , (bool Nothing (Just 9) . isPrefixOf "nine") ]
