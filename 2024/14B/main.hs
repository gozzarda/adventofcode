module Main where

import Data.Char (intToDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.List (elemIndex, group, sort, sortOn)
import Data.Maybe
import Data.Tuple (swap)
import Debug.Trace
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

type Vec2 = (Int, Int)

type Robot = (Vec2, Vec2)

type Prob = (Vec2, [Robot])

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

parseDimensions :: ReadP Vec2
parseDimensions = option (101, 103) $ do
  string "w="
  w <- parse
  skipSpaces
  string "h="
  h <- parse
  return (w, h)

parseVec2 :: ReadP Vec2
parseVec2 = do
  x <- parse
  char ','
  y <- parse
  return (x, y)

parseRobot :: ReadP Robot
parseRobot = do
  string "p="
  p <- parseVec2
  skipSpaces
  string "v="
  v <- parseVec2
  return (p, v)

parseProb :: ReadP Prob
parseProb = do
  wh <- parseDimensions
  skipSpaces
  pvs <- sepBy parseRobot skipSpaces
  return (wh, pvs)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (lf, ls) (rf, rs) = (f lf rf, f ls rs)

vadd :: Vec2 -> Vec2 -> Vec2
vadd = both2 (+)

vmul :: Vec2 -> Int -> Vec2
vmul v s = both (* s) v

vmod :: Vec2 -> Vec2 -> Vec2
vmod = both2 mod

solve :: Prob -> Soln
solve (wh, pvs) = trace (showVecs wh ps) r
  where
    (w, h) = wh
    psAt t = map (\(p, v) -> p `vadd` (v `vmul` t) `vmod` wh) pvs
    pss = map psAt [0 ..]
    (xns, yns) = unzip $ map (xyhists wh) pss
    xmi = clusterIndex $ take w xns
    ymi = clusterIndex $ take h yns
    (r, m) = crt [(xmi, w), (ymi, h)]
    ps = map (\(p, v) -> p `vadd` (v `vmul` r) `vmod` wh) pvs

xyhists :: Vec2 -> [Vec2] -> ([Int], [Int])
xyhists (w, h) ps = (histogram w xs, histogram h ys)
  where
    (xs, ys) = unzip $ nubOrd ps -- Only count one robot at each location

histogram :: Int -> [Int] -> [Int]
histogram n xs = map (pred . length) $ group $ sort $ [0 .. n - 1] ++ xs'
  where
    xs' = filter (\x -> 0 <= x && x < n) xs

-- The index into xss of the most clustered list
clusterIndex :: [[Int]] -> Int
clusterIndex xss = fromJust $ elemIndex (maximum mws) mws
  where
    n = minimum (map length xss) `div` 2
    mws = map (maxWindow n) xss

-- maxWindow n xs gives the maximum sum of any n-length window in maxWindow
maxWindow :: Int -> [Int] -> Int
maxWindow n xs = maximum windowSums
  where
    prefixSums = scanl (+) 0 xs
    windowSums = zipWith subtract prefixSums $ drop n prefixSums

crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
  where
    go (rl, ml) (rr, mr) = (mod r m, m)
      where
        r = rr + mr * (rl - rr) * inv mr ml
        m = mr * ml
    inv a m = s `mod` m
      where
        (_, s, _) = gcd' a m
    gcd' 0 b = (b, 0, 1)
    gcd' a b = (g, t - div b a * s, s)
      where
        (g, s, t) = gcd' (mod b a) a

gridHist :: Vec2 -> [Vec2] -> [[Int]]
gridHist (w, h) = go (0, 0) . sortOn swap . filter inBounds
  where
    inBounds (x, y) = 0 <= x && x < w && 0 <= y && y < h
    go (x, y) _ | y == h = []
    go (x, y) vs | x == w = [] : go (0, succ y) vs
    go (x, y) vs =
      let (xys, vs') = span (== (x, y)) vs
          (ns : nss) = go (succ x, y) vs'
       in (length xys : ns) : nss

showVecs :: Vec2 -> [Vec2] -> String
showVecs wh = unlines . (map . map) f . gridHist wh
  where
    f n = if n == 0 then '.' else intToDigit n
