module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust)

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

data Pipe = Src | NS | EW | NE | NW | SW | SE deriving (Ord, Eq)

toPipe :: Char -> Maybe Pipe
toPipe c = case c of
  'S' -> Just Src
  '|' -> Just NS
  '-' -> Just EW
  'L' -> Just NE
  'J' -> Just NW
  '7' -> Just SW
  'F' -> Just SE
  _ -> Nothing

type Coord = (Int, Int)

both2 :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
both2 f (lf, ls) (rf, rs) = (f lf rf, f ls rs)

coordAdd :: Coord -> Coord -> Coord
coordAdd = both2 (+)

coordSub :: Coord -> Coord -> Coord
coordSub = both2 (-)

pipeDeltas :: Pipe -> [Coord]
pipeDeltas p = case p of
  Src -> [(-1, 0), (0, -1), (0, 1), (1, 0)]
  NS -> [(-1, 0), (1, 0)]
  EW -> [(0, -1), (0, 1)]
  NE -> [(-1, 0), (0, 1)]
  NW -> [(-1, 0), (0, -1)]
  SW -> [(1, 0), (0, -1)]
  SE -> [(1, 0), (0, 1)]

pipesConnect :: (Coord, Pipe) -> (Coord, Pipe) -> Bool
pipesConnect (lc, lp) (rc, rp) = elem dlr lds && elem drl rds
  where
    dlr = coordSub rc lc
    drl = coordSub lc rc
    lds = pipeDeltas lp
    rds = pipeDeltas rp

solve :: Prob -> Soln
solve css = Map.foldl max 0 $ bfsDepth ipm srcs
  where
    pss = (map . map) toPipe css
    ipss = zipWith zip [[(r, c) | c <- [0 ..]] | r <- [0 ..]] pss
    ips = catMaybes $ map (uncurry (fmap . (,))) $ concat ipss
    ipm = Map.fromList ips
    srcs = [i | (i, p) <- ips, p == Src]

bfsDepth :: Map Coord Pipe -> [Coord] -> Map Coord Int
bfsDepth ipm = go 0 Map.empty
  where
    go _ idm [] = idm
    go d idm is = go (d + 1) idm' ns
      where
        is' = filter (`Map.notMember` idm) is
        idm' = Map.unionWith min idm $ Map.fromList $ zip is' $ repeat d
        ips = catMaybes $ map (flip lookupKey ipm) is'
        nss = map (\(rc, p) -> map (coordAdd rc) $ pipeDeltas p) ips
        npss = map (catMaybes . map (flip lookupKey ipm)) nss
        nps = concat $ zipWith (filter . pipesConnect) ips npss
        (ns, _) = unzip nps

lookupKey :: Ord k => k -> Map k v -> Maybe (k, v)
lookupKey k = fmap ((,) k) . Map.lookup k
