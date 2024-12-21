module Main where

import Data.Char (isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

type Prob = [[Char]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = lines

numLayers :: Int
numLayers = 2

solve :: Prob -> Soln
solve bss = sum (zipWith (*) ls ns)
  where
    initg = initGraph dirPad
    goalg = stepGraph numPad $ applyN numLayers (stepGraph dirPad) initg
    ls = map (sequenceCost goalg) bss
    ns = map (read . filter isDigit) bss

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f = id
applyN n f = f . applyN (pred n) f

type Coord = (Int, Int)

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (lf, ls) (rf, rs) = (f lf rf, f ls rs)

vadd :: Coord -> Coord -> Coord
vadd = both2 (+)

type Btn = Char

type Pad = (Map Coord Btn, Map Btn Coord)

mkPad :: [[Char]] -> Pad
mkPad bss = (Map.fromList rcbs, Map.fromList brcs)
  where
    rcbs = [((r, c), b) | (r, bs) <- zip [0 ..] bss, (c, b) <- zip [0 ..] bs, b /= ' ']
    brcs = map swap rcbs

dirPad :: Pad
dirPad = mkPad [" ^A", "<v>"]

numPad :: Pad
numPad = mkPad ["789", "456", "123", " 0A"]

-- Treat the robot entering button presses as a graph
-- w(u, v) is the cost to press v if we are currently pointing at u
type Graph v = (Set v, Map (v, v) Int)

-- At the operator level, all presses cost 1
initGraph :: Pad -> Graph Btn
initGraph pad = (vs, es)
  where
    (_, padbc) = pad
    vs = Map.keysSet padbc
    es = Map.fromList [((u, v), 1) | u <- Set.elems vs, v <- Set.elems vs]

-- Given a pad spec, determine its transition costs given its controller pad
stepGraph :: Pad -> Graph Btn -> Graph Btn
stepGraph pad oldg = (newvs, newewm)
  where
    (_, padbc) = pad
    (oldvs, oldewm) = oldg
    newvs = Map.keysSet padbc
    midvs = Set.cartesianProduct newvs oldvs
    midvews :: (Btn, Btn) -> [(((Btn, Btn), (Btn, Btn)), Int)]
    midvews (nv, ov) = mapMaybe mew (Set.elems oldvs)
      where
        mew ov' = liftA2 (ew ov') (doMove pad nv ov') (oldewm Map.!? (ov, ov'))
        ew ov' nv' w = (((nv, ov), (nv', ov')), w)
    midewm = Map.fromList $ concatMap midvews (Set.elems midvs)
    (_, midewm') = floydWarshall (midvs, midewm)
    newewm = Map.mapKeys bifst $ Map.filterWithKey bothA midewm'
      where
        bifst ((nv, _), (nv', _)) = (nv, nv')
        bothA ((_, ov), (_, ov')) _ = ov == 'A' && ov' == 'A'

doMove :: Pad -> Btn -> Btn -> Maybe Btn
doMove pad curr move = mrc' >>= (padcb Map.!?)
  where
    (padcb, padbc) = pad
    mrc' = case (padbc Map.!? curr, move) of
      (Just (r, c), '^') -> Just (r - 1, c)
      (Just (r, c), 'v') -> Just (r + 1, c)
      (Just (r, c), '<') -> Just (r, c - 1)
      (Just (r, c), '>') -> Just (r, c + 1)
      (Just (r, c), 'A') -> Just (r, c)
      _ -> Nothing

floydWarshall :: (Ord v) => Graph v -> Graph v
floydWarshall (vs, ewm) = (vs, foldl step ewm vs)
  where
    step ewm k = Map.unionWith min ewm ewm'
      where
        kd i j = liftA2 (+) (Map.lookup (i, k) ewm) (Map.lookup (k, j) ewm)
        ews = [fmap ((i, j),) (kd i j) | i <- Set.elems vs, j <- Set.elems vs]
        ewm' = Map.fromList (catMaybes ews)

-- Find the cost of a sequence of button presses starting from 'A'
sequenceCost :: Graph Btn -> [Btn] -> Int
sequenceCost g bs = sum $ map (ewm Map.!) es
  where
    (vs, ewm) = g
    es = zip ('A' : bs) bs
