module Main where

import Data.Char (digitToInt)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe

type Prob = [[Int]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = (map . map) digitToInt . lines

solve :: Prob -> Soln
solve hss = sum $ Map.intersection (pathCounts rchm) (Map.filter (== 0) rchm)
  where
    rchs = [((r, c), h) | (r, hs) <- zip [0 ..] hss, (c, h) <- zip [0 ..] hs]
    rchm = Map.fromList rchs

type Vec2 = (Int, Int)

dirs :: [Vec2]
dirs = [(-1, 0), (0, -1), (0, 1), (1, 0)]

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (lf, ls) (rf, rs) = (f lf rf, f ls rs)

pathCounts :: Map Vec2 Int -> Map Vec2 Int
pathCounts rchm = dpt
  where
    dpt = Map.mapWithKey dpf rchm
    dpf _ 9 = 1
    dpf rc h = sum $ mapMaybe (dpt Map.!?) ns
      where
        ns = filter (\rc' -> rchm Map.!? rc' == Just (h + 1)) $ map (both2 (+) rc) dirs
