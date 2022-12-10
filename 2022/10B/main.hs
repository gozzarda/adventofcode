module Main where

import Data.Bool (bool)
import Data.List (unfoldr)

data Oper = NoOp | AddX Int

type Case = [Oper]

type Soln = [[Bool]]

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readOper . lines

readOper :: String -> Oper
readOper s = case words s of
  ["noop"] -> NoOp
  ["addx", vs] -> AddX $ read vs

showSoln :: Soln -> String
showSoln = unlines . (map . map) (bool '.' '#')

type State = [Int]

initState :: State
initState = [1]

stepState :: State -> Oper -> State
stepState xs@(x : _) NoOp = x : xs
stepState xs@(x : _) (AddX v) = (x + v) : x : xs

statState :: State -> Soln
statState = map (zipWith (\i x -> abs (i - x) <= 1) [0 ..]) . chunksOf 40 . reverse . tail

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr (\xs -> if null xs then Nothing else Just $ splitAt n xs)

solve :: Case -> Soln
solve = statState . foldl stepState initState
