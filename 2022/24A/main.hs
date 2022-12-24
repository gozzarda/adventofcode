module Main where

import Control.Monad (foldM)
import Data.Bifunctor (bimap)
import Data.List (foldl', transpose)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

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

type Time = Int

type Board = ((Int, Int), Map Point Char)

makeBoard :: [[Char]] -> Board
makeBoard xss = ((h, w), m)
  where
    h = subtract 2 $ maximum $ map length $ transpose xss
    w = subtract 2 $ maximum $ map length xss
    m = M.fromList [((r, c), x) | (r, xs) <- zip [-1 ..] xss, (c, x) <- zip [-1 ..] xs, x /= '#']

boardCycle :: Board -> Int
boardCycle ((h, w), _) = h * w `div` gcd h w

boardSafe :: Board -> Time -> Point -> Bool
boardSafe ((h, w), m) t (r, c) = and [pcond, ucond, dcond, lcond, rcond]
  where
    pcond = M.member (r, c) m
    ucond = M.lookup (mod (r + t) h, c) m /= Just '^'
    dcond = M.lookup (mod (r - t) h, c) m /= Just 'v'
    lcond = M.lookup (r, mod (c + t) w) m /= Just '<'
    rcond = M.lookup (r, mod (c - t) w) m /= Just '>'

type State = (Point, Time)

type Adj = State -> [State]

makeAdj :: Board -> Adj
makeAdj b (p, t) = map (,t') ps'
  where
    t' = mod (t + 1) $ boardCycle b
    ps = map (both2 (+) p) [(-1, 0), (0, -1), (0, 0), (0, 1), (1, 0)]
    ps' = filter (boardSafe b t') ps

bfs :: (State -> [State]) -> (State -> Bool) -> State -> Either Int (Map State Int)
bfs adj done src = bfs' (M.singleton src 0) 0 [src]
  where
    bfs' dm d [] = Right dm
    bfs' dm d q = do
      let ss = concatMap adj q
      (dm', q') <- foldM f (dm, []) ss
      bfs' dm' (d + 1) q'
      where
        f _ s | done s = Left (d + 1)
        f (dm, q) s | M.member s dm = Right (dm, q)
        f (dm, q) s = Right (M.insert s (d + 1) dm, s : q)

solve :: Case -> Soln
solve xss = d
  where
    b@(_, pm) = makeBoard xss
    src = fst $ M.findMin pm
    dst = fst $ M.findMax pm
    done (p, _) = p == dst
    Left d = bfs (makeAdj b) done (src, 0)
