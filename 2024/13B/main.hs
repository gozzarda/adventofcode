module Main where

import Data.Maybe
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

type Vec2 = (Int, Int)

type Machine = (Vec2, Vec2, Vec2)

type Prob = [Machine]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = doReadP' $ parseProb <* eof

doReadP' :: ReadP a -> String -> a
doReadP' p = fromMaybe (error "Parsing failed") . doReadP p

doReadP :: ReadP a -> String -> Maybe a
doReadP p = fmap fst . listToMaybe . readP_to_S p

parse :: (Read a) => ReadP a
parse = ReadPrec.readPrec_to_P readPrec ReadPrec.minPrec

parseButton :: ReadP Vec2
parseButton = do
  string "Button "
  get
  string ": X+"
  x <- parse
  string ", Y+"
  y <- parse
  return (x, y)

parsePrize :: ReadP Vec2
parsePrize = do
  string "Prize: X="
  x <- parse
  string ", Y="
  y <- parse
  return (x + 10000000000000, y + 10000000000000)

parseMachine :: ReadP Machine
parseMachine = do
  axy <- parseButton
  skipSpaces
  bxy <- parseButton
  skipSpaces
  pxy <- parsePrize
  return (axy, bxy, pxy)

parseProb :: ReadP Prob
parseProb = sepBy parseMachine skipSpaces

acost :: Int
acost = 3

bcost :: Int
bcost = 1

solve :: Prob -> Soln
solve = sum . map cost . mapMaybe solveMachine

cost :: (Int, Int) -> Int
cost (x, y) = x * acost + y * bcost

-- Only works if axy, bxy, and pxy are not collinear (single solution),
-- but the input doesn't include any collinear cases so...
-- a * ax + b * bx = px
-- a * ay + b * by = py
-- b = (py - a * ay) / by
-- a * ax + (py - a * ay) * bx / by = px
-- a * ax * by + (py - a * ay) * bx = px * by
-- a * ax * by - a * ay * bx + py * bx = px * by
-- a * ax * by - a * ay * bx = px * by - py * bx
-- a * (ax * by - ay * bx) = px * by - py * bx
-- a = (px * by - py * bx) / (ax * by - ay * bx)
-- a = cross pxy bxy / cross axy bxy
solveMachine :: Machine -> Maybe (Int, Int)
solveMachine (axy, bxy, pxy) = if valid then Just (a, b) else Nothing
  where
    (ax, ay) = axy
    (bx, by) = bxy
    (px, py) = pxy
    cross (lx, ly) (rx, ry) = lx * ry - ly * rx
    n = cross pxy bxy
    d = cross axy bxy
    (a, ar) = n `divMod` d
    (b, br) = (py - a * ay) `divMod` by
    valid = d /= 0 && ar == 0 && br == 0 && a >= 0 && b >= 0
