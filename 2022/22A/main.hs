module Main where

import Data.Char
import Data.Foldable (find)
import Data.Function (on)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', groupBy, transpose)
import Data.Maybe

data Move = Walk Int | Turn Char

type Case = ([[Char]], [Move])

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase s = let ls = lines s in (init $ init ls, readMoves $ last ls)

readMoves :: String -> [Move]
readMoves = map readMove . groupBy ((==) `on` isNumber)

readMove :: String -> Move
readMove s | all isDigit s = Walk $ read s
readMove [c] = Turn c

showSoln :: Soln -> String
showSoln = unlines . return . show

type Context = (IntMap (IntMap Char), IntMap (IntMap Char))

makeContext :: [[Char]] -> Context
makeContext grid = (f $ map f' grid, f $ map f' $ transpose grid)
  where
    f = IM.fromList . zip [1 ..]
    f' = IM.filter (not . isSpace) . f

type Point = (Int, Int)

type State = (Point, Point)

turn' :: Point -> Char -> Point
turn' (r, c) 'L' = (-c, r)
turn' (r, c) 'R' = (c, -r)

turn :: State -> Char -> State
turn (p, d) c = (p, turn' d c)

lookupWrap :: Int -> IntMap a -> Maybe (Int, a)
lookupWrap _ m | IM.null m = Nothing
lookupWrap i m = Just $ maybe (i', v') (i,) $ IM.lookup i m
  where
    (il, vl) = IM.findMin m
    (iu, vu) = IM.findMax m
    (i', v') = if i < il then (iu, vu) else (il, vl)

step' :: IntMap Char -> Int -> Int -> Maybe Int
step' m i d = let d' = signum d in fmap fst $ find ((== '.') . snd) $ lookupWrap (i + d') m

step :: Context -> Point -> Point -> Maybe Point
step (rs, cs) (r, c) (0, d) = (r,) <$> step' (rs ! r) c d
step (rs, cs) (r, c) (d, 0) = (,c) <$> step' (cs ! c) r d

walk :: Context -> State -> Int -> State
walk _ s 0 = s
walk ctx (p, d) n = maybe (p, d) (\p -> walk ctx (p, d) (n - 1)) $ step ctx p d

move :: Context -> State -> Move -> State
move _ s (Turn c) = turn s c
move ctx s (Walk n) = walk ctx s n

stat :: State -> Int
stat ((r, c), d) = 1000 * r + 4 * c + f d
  where
    f (0, 1) = 0
    f (1, 0) = 1
    f (0, -1) = 2
    f (-1, 0) = 3

solve :: Case -> Soln
solve (grid, moves) = stat $ foldl' (move ctx) initState moves
  where
    ctx = makeContext grid
    (rs, _) = ctx
    (initr, cs) = IM.findMin rs
    (initc, _) = IM.findMin cs
    initState = ((initr, initc), (0, 1))
