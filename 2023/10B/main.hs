module Main where

import Data.List (intersect)
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

pipeNeighbours :: Coord -> Pipe -> [Coord]
pipeNeighbours rc p = map (coordAdd rc) $ pipeDeltas p

type AdjList = Map Coord [Coord]

transposeAdjList :: AdjList -> AdjList
transposeAdjList kvs = Map.fromListWith (++) [(v, [k]) | (k, vs) <- Map.assocs kvs, v <- vs]

pipesToAdjList :: [(Coord, Pipe)] -> AdjList
pipesToAdjList ips = Map.intersectionWith intersect insm $ transposeAdjList insm
  where
    insm = Map.fromList $ map (\(i, p) -> (i, pipeNeighbours i p)) ips

solve :: Prob -> Soln
solve css = (abs $ doublePolyArea path `div` 2) - (length path `div` 2) + 1
  where
    pss = (map . map) toPipe css
    ipss = zipWith zip [[(r, c) | c <- [0 ..]] | r <- [0 ..]] pss
    ips = catMaybes $ map (uncurry (fmap . (,))) $ concat ipss
    insm = pipesToAdjList ips
    src = head [i | (i, p) <- ips, p == Src]
    path = walkCycle insm src

walkCycle :: AdjList -> Coord -> [Coord]
walkCycle insm root = go Nothing root
  where
    go (Just _) curr | curr == root = []
    go mprev curr = curr : go (Just curr) (head $ filter ((/= mprev) . Just) $ (Map.!) insm curr)

doublePolyArea :: [Coord] -> Int
doublePolyArea ps = sum $ zipWith areaProd ps (tail $ cycle ps)

areaProd :: Coord -> Coord -> Int
areaProd (lx, ly) (rx, ry) = lx * ry - rx * ly
