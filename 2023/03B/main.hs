module Main where

import Data.Char (isDigit, isPunctuation, isSymbol)
import Data.Maybe (catMaybes)
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

type Pos = (Int, Int)

both2 :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
both2 f (ll, lr) (rl, rr) = (f ll rl, f lr rr)

solve :: Prob -> Soln
solve grid = sum $ catMaybes $ map (gearRatio nkss) gks
  where
    kvs = (zipWith zip) [[(r, c) | c <- [0 ..]] | r <- [0 ..]] grid
    gks = gearPositions kvs
    nkss = numberRegions kvs

gearPositions :: [[(Pos, Char)]] -> [Pos]
gearPositions kvs = map fst $ filter ((== '*') . snd) $ concat kvs

dilate :: Set Pos -> Set Pos
dilate ks = Set.unions $ map (flip Set.map ks . both2 (+)) deltas
  where
    deltas = [(dr, dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1]]

numberRegions :: [[(Pos, Char)]] -> [(Int, Set Pos)]
numberRegions = concat . map go
  where
    go [] = []
    go kvs = if null kds then go kvs'' else nk : go kvs''
      where
        (kds, kvs') = span (isDigit . snd) kvs
        kvs'' = dropWhile (not . isDigit . snd) kvs'
        (ks, ds) = unzip kds
        nk = (read ds, dilate $ Set.fromList ks)

gearRatio :: [(Int, Set Pos)] -> Pos -> Maybe Int
gearRatio nkss gk = if length nears == 2 then Just (product $ map fst nears) else Nothing
  where
    nears = filter (\(_, ks) -> Set.member gk ks) nkss
