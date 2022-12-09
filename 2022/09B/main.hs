module Main where

import Data.Set (Set)
import qualified Data.Set as Set

data Dir = U | D | L | R deriving (Read)

type Case = [(Dir, Int)]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readMove . lines

readMove :: String -> (Dir, Int)
readMove s = let [ds, ns] = words s in (read ds, read ns)

showSoln :: Soln -> String
showSoln = unlines . return . show

type Pos = (Int, Int)

type State = [Pos]

initState :: State
initState = replicate 10 (0, 0)

stepState :: State -> Dir -> State
stepState s@(h : ts) d = s'
  where
    s' = stepHead d h : zipWith stepTail s' ts

stepHead :: Dir -> Pos -> Pos
stepHead U (hx, hy) = (hx, hy + 1)
stepHead D (hx, hy) = (hx, hy - 1)
stepHead L (hx, hy) = (hx - 1, hy)
stepHead R (hx, hy) = (hx + 1, hy)

stepTail :: Pos -> Pos -> Pos
stepTail (hx, hy) t@(tx, ty) | abs (hx - tx) <= 1 && abs (hy - ty) <= 1 = t
stepTail (hx, hy) (tx, ty) = (tx + signum (hx - tx), ty + signum (hy - ty))

statState :: State -> Pos
statState = last

solve :: Case -> Soln
solve = Set.size . Set.fromList . map statState . scanl stepState initState . concatMap (uncurry $ flip replicate)
