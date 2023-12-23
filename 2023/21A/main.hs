module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

type Prob = [[Char]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = lines

numSteps :: Int
numSteps = 64

solve :: Prob -> Soln
solve tss = statState (steps !! numSteps)
  where
    steps = iterate stepState (initState tss)

type Coord = (Int, Int)

(.+.) :: Coord -> Coord -> Coord
(.+.) (ll, lr) (rl, rr) = (ll + rl, lr + rr)

type State = Map Coord Bool

initState :: [[Char]] -> State
initState tss = Map.fromList $ mapMaybe initAssoc $ concat itss
  where
    itss = zipWith zip [[(r, c) | c <- [0 ..]] | r <- [0 ..]] tss
    initAssoc (_, '#') = Nothing
    initAssoc (i, '.') = Just (i, False)
    initAssoc (i, 'S') = Just (i, True)

stepState :: State -> State
stepState inm = Map.mapWithKey orNeighbours inm
  where
    deltas = [(-1, 0), (0, -1), (0, 1), (1, 0)]
    orNeighbours k _ = or $ map (\d -> Map.findWithDefault False (k .+. d) inm) deltas

statState :: State -> Int
statState = Map.size . Map.filter id
