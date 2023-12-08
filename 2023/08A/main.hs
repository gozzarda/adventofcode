module Main where

import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Vert = String

data Turn = L | R deriving (Ord, Eq, Show, Read)

type Prob = ([Turn], [(Vert, (Vert, Vert))])

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb s = (readTurns tl, map readVert vls)
  where
    (tl : _ : vls) = lines s

readTurns :: String -> [Turn]
readTurns = map (read . (: []))

readVert :: String -> (Vert, (Vert, Vert))
readVert s = let (v : l : r : _) = filter (not . null) $ map (filter isAlphaNum) $ words s in (v, (l, r))

solve :: Prob -> Soln
solve (ts, vlrs) = length $ takeWhile (/= "ZZZ") path
  where
    vlrm = Map.fromList vlrs
    path = map fst $ iterate step ("AAA", cycle ts)
    step (v, (t : ts)) = (select t $ (Map.!) vlrm v, ts)

select :: Turn -> (a, a) -> a
select L (l, _) = l
select R (_, r) = r
