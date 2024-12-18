module Main where

import Data.Array.Unboxed
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

type Coord = (Int, Int)

type Prob = (Maybe Int, [Coord])

type Soln = Coord

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln (x, y) = unlines $ return (show x ++ "," ++ show y)

readProb :: String -> Prob
readProb = doReadP' $ parseProb <* eof

doReadP' :: ReadP a -> String -> a
doReadP' p = fromMaybe (error "Parsing failed") . doReadP p

doReadP :: ReadP a -> String -> Maybe a
doReadP p = fmap fst . listToMaybe . readP_to_S p

parse :: (Read a) => ReadP a
parse = ReadPrec.readPrec_to_P readPrec ReadPrec.minPrec

parseCoord :: ReadP Coord
parseCoord = do
  x <- parse
  char ','
  y <- parse
  return (x, y)

parseProb :: ReadP Prob
parseProb = do
  md <- option Nothing $ Just <$> (string "d=" *> parse)
  skipSpaces
  xys <- sepBy parseCoord skipSpaces
  return (md, xys)

solve :: Prob -> Soln
solve prob@(_, xys) = xys !! lowerBound (not . solve' prob) 0 (length xys)

solve' :: Prob -> Int -> Bool
solve' (md, xys) n = Map.member (d, d) xydm
  where
    d = fromMaybe 70 md
    xys' = take n xys
    xyBounds = ((0, 0), (d, d))
    grid = genArray xyBounds (const '.') // map (,'#') xys'
    xydm = bfs (adj grid) (0, 0)

lowerBound :: (Int -> Bool) -> Int -> Int -> Int
lowerBound f = go
  where
    go l u | l == u = l
    go l u = let m = (l + u + 1) `div` 2 in if f m then go l (m - 1) else go m u

type Vert = Coord

dirs :: [Coord]
dirs = [(-1, 0), (0, -1), (0, 1), (1, 0)]

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (lf, ls) (rf, rs) = (f lf rf, f ls rs)

vadd :: Coord -> Coord -> Coord
vadd = both2 (+)

adj :: UArray Vert Char -> Vert -> [Vert]
adj grid xy = filter ((== Just '.') . (grid !?)) (map (vadd xy) dirs)

bfs :: (Vert -> [Vert]) -> Vert -> Map Vert Int
bfs adj v = go Map.empty (Map.singleton v 0)
  where
    go :: Map Vert Int -> Map Vert Int -> Map Vert Int
    go dm udm | Map.null udm = dm
    go dm udm = go (Map.union dm udm) (Map.difference vdm dm)
      where
        vds = concatMap (\(u, d) -> map (,succ d) (adj u)) (Map.assocs udm)
        vdm = Map.fromList vds
