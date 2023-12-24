module Main where

import Data.List (findIndex, sortOn)
import Data.Maybe
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

import Debug.Trace

type Vec3 = (Integer, Integer, Integer)

type Hail = (Vec3, Vec3)

type Prob = [Hail]

type Soln = Integer

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

parse :: Read a => ReadP a
parse = ReadPrec.readPrec_to_P readPrec ReadPrec.minPrec

parseVec3 :: ReadP Vec3
parseVec3 = do
  x <- parse
  char ','
  skipSpaces
  y <- parse
  char ','
  skipSpaces
  z <- parse
  return (x, y, z)

parseHail :: ReadP Hail
parseHail = do
  pos <- parseVec3
  skipSpaces
  char '@'
  skipSpaces
  vel <- parseVec3
  return (pos, vel)

parseProb :: ReadP Prob
parseProb = endBy parseHail skipSpaces

solve :: Prob -> Soln
solve hails = sum $ take 3 $ traceShowId $ solveSystem eqs
  where
    eqs = concat $ zipWith pairSystem hails $ tail hails

-- forall i : p_i + v_i t_i = p_s + v_s t_i
-- => p_i - p_s = (v_s - v_i) t_i
-- => (p_i - p_s) X (v_s - v_i) = 0
-- => p_i X v_s - p_i X v_i - p_s X v_s + p_s X v_i = 0
-- - (p_j X v_s - p_j X v_j - p_s X v_s + p_s X v_j = 0)
-- => (p_i - p_j) X v_s + p_j X v_j - p_i X v_i + p_s X (v_i - v_j) = 0
-- => (p_i - p_j) X v_s + p_s X (v_i - v_j) = p_i X v_i - p_j X v_j
-- let dp_ij = p_i - p_j, dv_ij = v_i - v_j
-- => dp_ij X v_s + p_s X dv_ij = p_i X v_i - p_j X v_j
-- dp_ij X v_s = (dp_ijy v_sz - dp_ijz v_sy) x + (dp_ijz v_sx - dp_ijx v_sz) y + (dp_ijx v_sy - dp_ijy v_sx) z
-- p_s X dv_ij = (p_sy dv_ijz - p_sz dv_ijy) x + (p_sz dv_ijx - p_sx dv_ijz) y + (p_sx dv_ijy - p_sy dv_ijx) z
--   p_sx     p_sy     p_sz     v_sx     v_sy     v_sz   |  1
--    0      dv_ijz  -dv_ijy     0     -dp_ijz   dp_ijy  |  (p_i X v_i - p_j X v_j)_x
-- -dv_ijz     0      dv_ijx   dp_ijz     0     -dp_ijx  |  (p_i X v_i - p_j X v_j)_y
--  dv_ijy  -dv_ijx     0     -dp_ijy   dp_ijx     0     |  (p_i X v_i - p_j X v_j)_z

elementwise3 :: (a -> b -> c) -> (a, a, a) -> (b, b, b) -> (c, c, c)
elementwise3 f (lx, ly, lz) (rx, ry, rz) = (f lx rx, f ly ry, f lz rz)

(.-.) :: Vec3 -> Vec3 -> Vec3
(.-.) = elementwise3 (-)

cross :: Vec3 -> Vec3 -> Vec3
cross (lx, ly, lz) (rx, ry, rz) = (ly * rz - lz * ry, lz * rx - lx * rz, lx * ry - ly * rx)

pairSystem :: Hail -> Hail -> [[Integer]]
pairSystem (pl, vl) (pr, vr) =
  [ [0, dvz, -dvy, 0, -dpz, dpy, cx],
    [-dvz, 0, dvx, dpz, 0, -dpx, cy],
    [dvy, -dvx, 0, -dpy, dpx, 0, cz]
  ]
  where
    (cx, cy, cz) = (pl `cross` vl) .-. (pr `cross` vr)
    (dpx, dpy, dpz) = pl .-. pr
    (dvx, dvy, dvz) = vl .-. vr

solveSystem :: [[Integer]] -> [Integer]
solveSystem = map ((\[d, n] -> n `div` d) . filter (/= 0)) . sortOn (findIndex (/= 0)) . gaussElim

gaussElim :: [[Integer]] -> [[Integer]]
gaussElim = filter (any (/= 0)) . go [] . go []
  where
    go pref suff = case suff of
      [] -> pref
      (xs : xss) -> go pref' suff'
        where
          pref' = xs : map (elimRow xs) pref
          suff' = map (elimRow xs) xss

elimRow :: [Integer] -> [Integer] -> [Integer]
elimRow ls rs = case listToMaybe $ catMaybes $ init $ zipWith mcoeffs ls rs of
  Nothing -> rs
  Just (cl, cr) -> reduceRow $ zipWith subtract (map (* cl) ls) (map (* cr) rs)
  where
    mcoeffs 0 _ = Nothing
    mcoeffs _ 0 = Nothing
    mcoeffs l r = let m = lcm l r in Just (m `div` l, m `div` r)

reduceRow :: [Integer] -> [Integer]
reduceRow (x : xs) = let d = foldl gcd x xs in if d == 0 then (x : xs) else map (`div` d) (x : xs)
