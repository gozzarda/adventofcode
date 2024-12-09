module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

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

solve :: Prob -> Soln
solve xss = Set.size $ Set.intersection arcs $ Set.unions $ map ansf rcss
  where
    xrcs = [(x, (r, c)) | (r, xs) <- zip [0 ..] xss, (c, x) <- zip [0 ..] xs]
    arcs = Set.fromList $ map snd xrcs
    xrcm = Map.fromListWith Set.union $ map (fmap Set.singleton) xrcs
    ercs = Map.findWithDefault Set.empty '.' xrcm
    rcss = Map.elems $ Map.delete '.' xrcm
    ansf rcs = Set.difference (Set.fromList ans) rcs
      where
        rcs' = Set.toList rcs
        ans = [(r + (r - r'), c + (c - c')) | (r, c) <- rcs', (r', c') <- rcs']
