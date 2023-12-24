module Main where

import Data.Bifunctor (bimap)
import Data.List (tails)
import Data.Maybe
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

type Vec3 = (Integer, Integer, Integer)

type Hail = (Vec3, Vec3)

type Prob = [Hail]

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
solve hails = length $ filter vecInBounds $ intersections
  where
    pairs = concatMap (\(x : xs) -> map ((,) x) xs) $ filter (not . null) $ tails $ map fromHail hails
    intersections = mapMaybe (uncurry lineIntersection) pairs

inBounds :: Rational -> Bool
inBounds x = 200000000000000 <= x && x <= 400000000000000

vecInBounds :: Vec2 -> Bool
vecInBounds = (uncurry (&&)) . bimap inBounds inBounds

type Vec2 = (Rational, Rational)

fromVec3 :: Vec3 -> Vec2
fromVec3 (x, y, _) = (fromIntegral x, fromIntegral y)

elementwise2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
elementwise2 f (lx, ly) (rx, ry) = (f lx rx, f ly ry)

(.+.) :: Vec2 -> Vec2 -> Vec2
(.+.) = elementwise2 (+)

(.-.) :: Vec2 -> Vec2 -> Vec2
(.-.) = elementwise2 (-)

(.*.) :: Vec2 -> Vec2 -> Rational
(.*.) (lx, ly) (rx, ry) = lx * rx + ly * ry

(.*) :: Vec2 -> Rational -> Vec2
(.*) (x, y) s = (x * s, y * s)

(./) :: Vec2 -> Rational -> Vec2
(./) (x, y) s = (x / s, y / s)

areaProd :: Vec2 -> Vec2 -> Rational
areaProd (lx, ly) (rx, ry) = lx * ry - ly * rx

type Ray = (Vec2, Vec2)

fromHail :: Hail -> Ray
fromHail = bimap fromVec3 fromVec3

lineIntersection :: Ray -> Ray -> Maybe Vec2
lineIntersection (lp, lv) (rp, rv) = if valid then Just (n ./ d) else Nothing
  where
    le = lp .+. lv
    re = rp .+. rv
    n = (lv .* (rp `areaProd` re)) .-. (rv .* (lp `areaProd` le))
    d = lv `areaProd` rv
    lfuture = (n .-. (lp .* d)) .*. (lv .* d) >= 0
    rfuture = (n .-. (rp .* d)) .*. (rv .* d) >= 0
    valid = d /= 0 && lfuture && rfuture
