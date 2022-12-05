module Main where

import Data.Bifunctor (bimap)
import Data.Char (isNumber, isUpper)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (transpose)

type Crate = Char

type Stack = [Crate]

type Move = (Int, Int, Int)

type Case = ([Stack], [Move])

type Soln = [Crate]

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = bimap readStacks readMoves . splitBy null . lines

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy f = fmap (dropWhile f) . break f

readStacks :: [String] -> [Stack]
readStacks = map (filter isUpper) . filter (isNumber . last) . transpose

readMoves :: [String] -> [Move]
readMoves = map readMove

readMove :: String -> Move
readMove s = let [num, src, dst] = map read $ filter (isNumber . head) $ words s in (num, src, dst)

showSoln :: Soln -> String
showSoln = unlines . return

type State = IntMap Stack

initState :: [Stack] -> State
initState = IntMap.fromList . zip [1 ..]

stepState :: State -> Move -> State
stepState st (num, src, dst) = IntMap.adjust (crates ++) dst $ IntMap.adjust (drop num) src st
  where
    crates = reverse $ take num $ IntMap.findWithDefault undefined src st

statState :: State -> Soln
statState = map head . IntMap.elems

solve :: Case -> Soln
solve (stacks, moves) = statState $ foldl stepState (initState stacks) moves
