module Main where

import Data.Function (on)
import Data.List (groupBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type Prob = ([[Char]], [Char])

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = fmap concat . splitBy null . lines

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy f xs =
  let (pref, suff) = break f xs
   in (pref, dropWhile f suff)

solve :: Prob -> Soln
solve (xss, is) = evalState $ foldl stepState (initState xss') is
  where
    xss' = map (concatMap expand) xss
    expand '#' = "##"
    expand 'O' = "[]"
    expand '.' = ".."
    expand '@' = "@."

type Coord = (Int, Int)

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (lf, ls) (rf, rs) = (f lf rf, f ls rs)

vadd :: Coord -> Coord -> Coord
vadd = both2 (+)

-- Could speed up lookups by maintaining row- and col-major sets of empty space,
-- but not worth it for such small input.
type State = (Coord, Map Coord Char)

initState :: [[Char]] -> State
initState xss = (bot, rcxm)
  where
    rcxs = [((r, c), x) | (r, xs) <- zip [0 ..] xss, (c, x) <- zip [0 ..] xs]
    bot = case filter ((== '@') . snd) rcxs of
      ((rc, _) : _) -> rc
      _ -> error "No robot found in input grid"
    rcxm = Map.insert bot '.' $ Map.fromList rcxs

showState :: State -> String
showState (bot, rcxm) = unlines $ (map . map) snd rcxss
  where
    rcxm' = Map.insert bot '@' rcxm
    rcxss = groupBy ((==) `on` (fst . fst)) (Map.assocs rcxm')

traceState :: State -> State
traceState s = trace (showState s) s

dir :: Char -> Coord
dir '^' = (-1, 0)
dir '<' = (0, -1)
dir '>' = (0, 1)
dir 'v' = (1, 0)

stepState :: State -> Char -> State
stepState (bot, rcxm) i = case dfs bot' (Just Set.empty) of
  Nothing -> (bot, rcxm)
  (Just seen) -> (vadd drc bot, Map.union delta rcxm)
    where
      boxes = Map.mapKeys (vadd drc) $ Map.restrictKeys rcxm seen
      delta = Map.union boxes $ Map.fromSet (const '.') seen
  where
    drc = dir i
    bot' = vadd drc bot
    dfs _ Nothing = Nothing
    dfs rc (Just seen) | Set.member rc seen = Just seen
    dfs rc@(r, c) (Just seen) = case Map.lookup rc rcxm of
      Just '#' -> Nothing
      Just '[' -> dfs (r, c + 1) $ dfs (vadd rc drc) $ Just (Set.insert rc seen)
      Just ']' -> dfs (r, c - 1) $ dfs (vadd rc drc) $ Just (Set.insert rc seen)
      _ -> Just seen

evalState :: State -> Int
evalState (_, rcxm) = sum $ Map.mapWithKey f $ Map.filter (== '[') rcxm
  where
    f (r, c) _ = 100 * r + c
