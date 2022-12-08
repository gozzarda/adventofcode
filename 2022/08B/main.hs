module Main where

import Data.Char (digitToInt)
import qualified Data.IntMap as IntMap
import Data.List (transpose)
import Data.Maybe (fromMaybe)

type Case = [[Int]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map (map digitToInt) . lines

showSoln :: Soln -> String
showSoln = unlines . return . show

solve :: Case -> Soln
solve xss = maximum $ concat gridScores
  where
    rotrs = [id, map reverse . transpose, map reverse . reverse, transpose . map reverse]
    rotls = id : reverse (tail rotrs)
    mapRotR f x = zipWith (\t' t -> t' $ f $ t x) rotls rotrs
    gridCardinalDists = mapRotR (map viewDistsL) xss
    gridScores = foldl1 ((zipWith . zipWith) (*)) gridCardinalDists

viewDistsL :: [Int] -> [Int]
viewDistsL xs = zipWith (\(x, i) hm -> i - hit x hm) xis hms
  where
    xis = zip xs [0 ..]
    hms = scanl (\m (k, v) -> IntMap.insert k v m) IntMap.empty xis
    hit x m = let (_, p, m') = IntMap.splitLookup x m in IntMap.foldl' max (fromMaybe 0 p) m'
