{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

data Resource = Ore | Clay | Obsidian | Geode deriving (Eq, Ord, Enum, Bounded)

data Blueprint = Blueprint Int (Map Resource (Map Resource Int)) deriving (Show)

type Case = [Blueprint]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = doReadP' parseCase

doReadP' :: ReadP a -> String -> a
doReadP' p = fromMaybe (error "Parsing failed") . doReadP p

doReadP :: ReadP a -> String -> Maybe a
doReadP p = fmap fst . listToMaybe . readP_to_S p

parse :: Read a => ReadP a
parse = ReadPrec.readPrec_to_P readPrec ReadPrec.minPrec

parseShow :: (Show a, Enum a, Bounded a) => ReadP a
parseShow = choice $ map (\x -> string (show x) >> return x) [minBound .. maxBound]

instance Show Resource where
  show Ore = "ore"
  show Clay = "clay"
  show Obsidian = "obsidian"
  show Geode = "geode"

parseResource :: ReadP Resource
parseResource = parseShow

parseResourceAmount :: ReadP (Int, Resource)
parseResourceAmount = do
  amt <- parse
  skipSpaces
  res <- parseResource
  return (amt, res)

parseRobotCost :: ReadP (Map Resource Int)
parseRobotCost = do
  let part = swap <$> parseResourceAmount
  let sep = skipSpaces *> string "and" <* skipSpaces
  M.fromListWith (+) <$> sepBy1 part sep

parseRobot :: ReadP (Resource, Map Resource Int)
parseRobot = do
  string "Each" >> skipSpaces
  res <- parseResource
  skipSpaces >> string "robot costs" >> skipSpaces
  cost <- parseRobotCost
  char '.'
  return (res, cost)

parseBlueprint :: ReadP Blueprint
parseBlueprint = do
  string "Blueprint" >> skipSpaces
  idn <- parse
  char ':' >> skipSpaces
  rs <- sepBy1 parseRobot skipSpaces
  return $ Blueprint idn $ M.fromList rs

parseCase :: ReadP [Blueprint]
parseCase = sepBy1 parseBlueprint skipSpaces <* eof

showSoln :: Soln -> String
showSoln = unlines . return . show

resmapLE :: Map Resource Int -> Map Resource Int -> Bool
resmapLE ls rs = and $ M.elems $ M.intersectionWith (<=) ls' rs'
  where
    ls' = M.union ls $ M.map (const 0) rs
    rs' = M.union rs $ M.map (const 0) ls

type Robots = Map Resource Int

maxRobots :: Blueprint -> Robots
maxRobots (Blueprint _ rm) = M.insert Geode maxBound $ M.unionsWith max $ M.elems rm

type Stocks = Map Resource Int

stocksNN :: Stocks -> Bool
stocksNN = all (>= 0) . M.elems

resources :: [Resource]
resources = [minBound .. maxBound]

type State = (Int, Robots, Stocks)

tick :: State -> State
tick (t, rs, ss) = (t - 1, rs, M.unionWith (+) ss rs)

makeRobot :: Blueprint -> State -> Resource -> Maybe State
makeRobot bp (t, rs, ss) r = if valid then Just s' else Nothing
  where
    Blueprint _ rm = bp
    cm = maxRobots bp
    rc = rm ! r
    rs' = M.insertWith (+) r 1 rs
    ss' = M.unionsWith (+) [ss, rs, M.map negate rc]
    s' = (t - 1, rs', ss')
    tcond = t > 0
    rcond = resmapLE rs' cm
    scond = resmapLE rc ss
    valid = tcond && rcond && scond

makeRobotASAP :: Blueprint -> State -> Resource -> Maybe State
makeRobotASAP bp s r = listToMaybe ticks'
  where
    ticks = takeWhile (\(t, _, _) -> t > 0) $ iterate tick s
    ticks' = mapMaybe (\s -> makeRobot bp s r) ticks

makeRobotsASAP :: Blueprint -> State -> Map Resource State
makeRobotsASAP bp s = maybe rsm (M.singleton Geode) mgr
  where
    mgr = makeRobot bp s Geode
    rrm = M.fromList $ zip resources resources
    rsm = M.mapMaybe (makeRobotASAP bp s) rrm

limit :: State -> Int
limit (t, rs, ss) = s + r * t + triangle
  where
    s = M.findWithDefault 0 Geode ss
    r = M.findWithDefault 0 Geode rs
    triangle = t * (t - 1) `div` 2

dfs :: Blueprint -> State -> Int
dfs bp = dfs' 0
  where
    dfs' :: Int -> State -> Int
    dfs' best state | limit state < best = best
    dfs' best state = best''
      where
        (t, rs, ss) = state
        kids = makeRobotsASAP bp state
        gr = M.findWithDefault 0 Geode rs
        gs = M.findWithDefault 0 Geode ss
        base = t * gr + gs
        best' = max best base
        best'' = M.foldl dfs' best' kids

initState :: State
initState = (24, M.singleton Ore 1, M.empty)

quality :: Blueprint -> Int
quality bp = i * best
  where
    (Blueprint i _) = bp
    best = dfs bp initState

solve :: Case -> Soln
solve = sum . map quality
