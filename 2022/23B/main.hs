module Main where

import Control.Applicative ((<|>))
import Data.Bifunctor (bimap)
import Data.List (elemIndex, foldl', tails)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

type Case = [[Char]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = lines

showSoln :: Soln -> String
showSoln = unlines . return . show

type Point = (Int, Int)

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f = uncurry bimap . both f

type State = Set Point

makeState :: [[Char]] -> State
makeState xss = S.fromList [(r, c) | (r, xs) <- zip [0 ..] xss, (c, x) <- zip [0 ..] xs, x == '#']

type Option = State -> Point -> Maybe Point

makeOption :: [Point] -> Point -> Option
makeOption looks move ps p = if all ((`S.notMember` ps) . both2 (+) p) looks then Just $ both2 (+) move p else Nothing

joinOption :: Option -> Option -> Option
joinOption l r ps p = l ps p <|> r ps p

options :: [Option]
options = cycle $ map (foldl' joinOption stay . take 4) (take 4 $ tails $ cycle [north, south, west, east])
  where
    deltas = [(dr, dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], (dr, dc) /= (0, 0)]
    stay = makeOption deltas (0, 0)
    north = makeOption [(-1, -1), (-1, 0), (-1, 1)] (-1, 0)
    south = makeOption [(1, -1), (1, 0), (1, 1)] (1, 0)
    west = makeOption [(-1, -1), (0, -1), (1, -1)] (0, -1)
    east = makeOption [(-1, 1), (0, 1), (1, 1)] (0, 1)

proposals :: Option -> State -> Map Point [Point]
proposals f s = M.fromListWith (++) $ map (\p -> (fromMaybe p $ f s p, [p])) $ S.elems s

moves :: Map Point [Point] -> State
moves m = S.union move' stay'
  where
    (move, stay) = M.partition (\ps -> length ps == 1) m
    move' = M.keysSet move
    stay' = S.unions $ map S.fromList $ M.elems stay

turn :: State -> Option -> State
turn s f = moves $ proposals f s

turns :: State -> [State]
turns s = scanl turn s options

solve :: Case -> Soln
solve xss = (+ 1) $ fromJust $ elemIndex True $ zipWith (==) ss $ tail ss
  where
    s = makeState xss
    ss = turns s
