module Main where

import Data.List (elemIndex)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
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
solve (wh, pvs) = trace (drawVecs wh $ Set.fromList $ pss !! i) i
  where
    (w, h) = wh
    wm = w `div` 2
    hm = h `div` 2
    m = (wm, hm)
    (ps, vs) = unzip pvs
    pss = take (w * h) $ iterate (zipWith (\v p -> p `vadd` v `vmod` wh) vs) ps
    biggests = map (maximum . blobSizes . Set.fromList) pss
    biggest = maximum biggests
    i = fromJust $ elemIndex biggest biggests

dirs :: [Vec2]
dirs = [(-1, 0), (0, -1), (0, 1), (1, 0)]

blobSizes :: Set Vec2 -> [Int]
blobSizes vs = go vs
  where
    go vs = case Set.lookupMin vs of
      Nothing -> []
      Just v -> let (vs', n) = dfs (vs, 0) v in n : go vs'
      where
        dfs (vs, n) v | Set.notMember v vs = (vs, n)
        dfs (vs, n) v = foldl dfs (Set.delete v vs, succ n) (map (both2 (+) v) dirs)

drawVecs :: Vec2 -> Set Vec2 -> String
drawVecs (w, h) vs = unlines [[if Set.member (x, y) vs then 'X' else '.' | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]
