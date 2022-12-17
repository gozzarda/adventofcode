{-# LANGUAGE RankNTypes #-}

module Main where

import Data.List (group, sort)
import Data.Maybe
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

type Point = (Int, Int)

type Case = (Int, [(Point, Point)])

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase s = (qy, rs)
  where
    (ql : rls) = lines s
    qy = fromJust $ doReadP parseQuery ql
    rs = map (fromJust . doReadP parseReport) rls

doReadP :: ReadP a -> String -> Maybe a
doReadP p = fmap fst . listToMaybe . readP_to_S p

parse :: Read a => ReadP a
parse = ReadPrec.readPrec_to_P readPrec ReadPrec.minPrec

parseCoord :: String -> ReadP Int
parseCoord label = string (label ++ "=") *> parse

parseQuery :: ReadP Int
parseQuery = string "Count positions where a beacon cannot be present at " *> parseCoord "y"

parsePoint :: ReadP Point
parsePoint = do
  x <- parseCoord "x"
  string ", "
  y <- parseCoord "y"
  return (x, y)

parseReport :: ReadP (Point, Point)
parseReport = do
  string "Sensor at "
  sensor <- parsePoint
  string ": closest beacon is at "
  beacon <- parsePoint
  return (sensor, beacon)

showSoln :: Soln -> String
showSoln = unlines . return . show

manhattan :: Point -> Point -> Int
manhattan (lx, ly) (rx, ry) = abs (lx - rx) + abs (ly - ry)

type Range = (Int, Int)

diamondRange :: Int -> Point -> Int -> Maybe Range
diamondRange y (sx, sy) d = if d < dy then Nothing else Just (sx - dx, sx + dx + 1)
  where
    dy = abs (y - sy)
    dx = d - dy

countCovered :: [Range] -> Int
countCovered lus = sum $ zipWith (*) hs ws
  where
    (ls, us) = unzip lus
    (ts, ds) = unzip $ sort $ map (,1) ls ++ map (,-1) us
    hs = map signum $ scanl1 (+) ds
    ws = zipWith subtract ts (tail ts)

solve :: Case -> Soln
solve (qy, rs) = countCovered lus - length qbs
  where
    (ss, bs) = unzip rs
    ds = zipWith manhattan ss bs
    lus = catMaybes $ zipWith (diamondRange qy) ss ds
    qbs = map head $ group $ sort $ filter ((== qy) . snd) bs
