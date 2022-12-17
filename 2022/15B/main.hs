{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Bifunctor (bimap, first)
import Data.List (group, sort)
import Data.Maybe
import Data.Semigroup (Min (..))
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
readCase s = (lim, rs)
  where
    (liml : rls) = lines s
    lim = fromJust $ doReadP parseLimit liml
    rs = map (fromJust . doReadP parseReport) rls

doReadP :: ReadP a -> String -> Maybe a
doReadP p = fmap fst . listToMaybe . readP_to_S p

parse :: Read a => ReadP a
parse = ReadPrec.readPrec_to_P readPrec ReadPrec.minPrec

parseLimit :: ReadP Int
parseLimit = string "Maximum coordinate is " *> parse

parseCoord :: String -> ReadP Int
parseCoord label = string (label ++ "=") *> parse

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

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

-- Leaf index value
-- Branch lower upper delta value ltree rtree
data SegTree = Leaf Int Int | Branch Int Int Int (Int, Int) SegTree SegTree

segTree :: Int -> Int -> SegTree
segTree l u | l >= u = error "Empty segment"
segTree l u | (u - l) == 1 = Leaf l 0
segTree l u = let m = (l + u) `div` 2 in Branch l u 0 (0, l) (segTree l m) (segTree m u)

segTreeVal :: SegTree -> (Int, Int)
segTreeVal (Leaf i v) = (v, i)
segTreeVal (Branch _ _ d (v, i) _ _) = (d + v, i)

segTreeAdd :: Int -> Int -> Int -> SegTree -> SegTree
segTreeAdd ql qu qd (Leaf i v) | ql <= i && i < qu = Leaf i (v + qd)
segTreeAdd ql qu qd (Branch l u d v lt rt) | ql <= l && u <= qu = Branch l u (d + qd) v lt rt
segTreeAdd ql qu qd (Branch l u d v lt rt) | l < qu && ql < u = Branch l u d v' lt' rt'
  where
    (lt', rt') = both (segTreeAdd ql qu qd) (lt, rt)
    v' = min (segTreeVal lt') (segTreeVal rt')
segTreeAdd _ _ _ t = t

segTreeMin :: Int -> Int -> SegTree -> Maybe (Int, Int)
segTreeMin ql qu st = getMin <$> segTreeMin' st
  where
    segTreeMin' :: SegTree -> Maybe (Min (Int, Int))
    segTreeMin' (Leaf i v) | ql <= i && i < qu = Just $ Min (v, i)
    segTreeMin' (Branch l u d (v, i) _ _) | ql <= l && u <= qu = Just $ Min (d + v, i)
    segTreeMin' (Branch l u d _ lt rt) | l < qu && ql < u = (fmap . fmap) (first (d +)) $ segTreeMin' lt <> segTreeMin' rt
    segTreeMin' _ = Nothing

-- AddOp lower upper delta
-- MinOp lower upper
data Oper = AddOp Int Int Int | MinOp Int Int deriving (Eq, Ord)

operRange :: Oper -> (Int, Int)
operRange (AddOp l u _) = (l, u)
operRange (MinOp l u) = (l, u)

doOper :: SegTree -> Oper -> (Maybe (Int, Int), SegTree)
doOper st (AddOp l u d) = (Nothing, segTreeAdd l u d st)
doOper st (MinOp l u) = (segTreeMin l u st, st)

type Event = (Int, Oper)

doEvents :: [Event] -> [(Int, Maybe (Int, Int))]
doEvents es = zip ts mvws
  where
    (ts, os) = unzip $ sort es
    (l, u) = bimap minimum maximum $ unzip $ map operRange os
    mvws = map fst $ tail $ scanl (doOper . snd) (Nothing, segTree l u) os

fromXYtoTW :: Point -> Point
fromXYtoTW (x, y) = (x + y, x - y)

fromTWtoXY :: Point -> Point
fromTWtoXY (t, w) = both (`div` 2) (t + w, t - w)

manhattan :: Point -> Point -> Int
manhattan (lx, ly) (rx, ry) = abs (lx - rx) + abs (ly - ry)

reportEvents :: Point -> Point -> (Event, Event)
reportEvents s b = ((tl, AddOp wl wu 1), (tu, AddOp wl wu (-1)))
  where
    d = manhattan s b
    s' = fromXYtoTW s
    (tl, wl) = both (subtract d) s'
    (tu, wu) = both (+ (d + 1)) s'

queryEvents :: Int -> [Int] -> [Event]
queryEvents lim ts = map (f . head) $ group $ sort $ ts ++ map (subtract 1) ts
  where
    f t = let w = min t (2 * lim - t) in (t, MinOp (-w) (w + 1))

solve :: Case -> Soln
solve (lim, rs) = let (x, y) = head xys in 4000000 * x + y
  where
    res = uncurry (++) $ unzip $ map (uncurry reportEvents) rs
    qes = queryEvents lim $ map fst res
    tmvws = doEvents $ sort $ res ++ qes
    tvws = mapMaybe (uncurry $ fmap . (,)) tmvws
    tws = mapMaybe (\(t, (v, w)) -> if v == 0 then Just (t, w) else Nothing) tvws
    xys = map fromTWtoXY tws
