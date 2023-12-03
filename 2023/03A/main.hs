module Main where

import Data.Char (isDigit, isPunctuation, isSymbol)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

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
solve grid = sum $ map fst $ filter (\(_, ks) -> not $ Set.disjoint sks $ Set.fromList ks) nkss
  where
    kvs = (zipWith zip) [[(r, c) | c <- [0 ..]] | r <- [0 ..]] grid
    sks = dilate $ symbolPositions kvs
    nkss = numberPositions kvs

symbolPositions :: [[(Pos, Char)]] -> Set Pos
symbolPositions kvs = Set.fromList $ map fst $ filter (isPart . snd) $ concat kvs
  where
    isPart c = c /= '.' && isPunctuation c || isSymbol c

dilate :: Set Pos -> Set Pos
dilate ks = Set.unions $ map (flip Set.map ks . both2 (+)) deltas
  where
    deltas = [(dr, dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1]]

numberPositions :: [[(Pos, Char)]] -> [(Int, [Pos])]
numberPositions = concat . map go
  where
    go [] = []
    go kvs = if null kds then go kvs'' else nk : go kvs''
      where
        (kds, kvs') = span (isDigit . snd) kvs
        kvs'' = dropWhile (not . isDigit . snd) kvs'
        nk = swap $ fmap read $ unzip kds
