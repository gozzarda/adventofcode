module Main where

import Data.List (sortOn)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

type Vec3 = (Int, Int, Int)

type Block = (Vec3, Vec3)

type Prob = [Block]

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
  y <- parse
  char ','
  z <- parse
  return (x, y, z)

parseBlock :: ReadP Block
parseBlock = do
  lhs <- parseVec3
  char '~'
  rhs <- parseVec3
  return (lhs, rhs)

parseProb :: ReadP Prob
parseProb = endBy parseBlock skipSpaces

solve :: Prob -> Soln
solve blocks = sum $ Map.map Set.size $ dp adj
  where
    blocks' = sortOn (\((_, _, lz), (_, _, rz)) -> min lz rz) blocks
    (_, adj) = foldl dropBlock initState blocks'

type Vec2 = (Int, Int)

-- (Highest block for each (x, y), AdjList of supports)
type State = (Map Vec2 Block, Map Block (Set Block))

initState :: State
initState = (Map.empty, Map.empty)

dropBlock :: State -> Block -> State
dropBlock (xybm, adj) block = (xybm', adj')
  where
    ((lx, ly, lz), (rx, ry, rz)) = block
    shadow = blockShadow block
    belows = Map.restrictKeys xybm shadow
    height = Map.foldl' max 1 $ Map.map blockTop belows
    supports = Set.fromList $ filter ((== height) . blockTop) $ Map.elems belows
    block' = let dz = height - min lz rz in ((lx, ly, lz + dz), (rx, ry, rz + dz))
    xybm' = Map.union (Map.fromSet (const block') shadow) xybm
    adj' = Map.insert block' supports adj

blockShadow :: Block -> Set Vec2
blockShadow ((lx, ly, _), (rx, ry, _)) = Set.fromList [(x, y) | x <- xs, y <- ys]
  where
    xs = [min lx rx .. max lx rx]
    ys = [min ly ry .. max ly ry]

blockTop :: Block -> Int
blockTop ((_, _, lz), (_, _, rz)) = 1 + max lz rz

dp :: Map Block (Set Block) -> Map Block (Set Block)
dp adj = Map.mapWithKey Set.delete dpt
  where
    dpt = Map.mapWithKey dpf adj
    dpf k v = Set.insert k $ intersections (Map.restrictKeys dpt v)

intersections :: (Foldable f, Ord a) => f (Set a) -> Set a
intersections = fromMaybe Set.empty . foldl ((Just .) . maybe id Set.intersection) Nothing
