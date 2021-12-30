{-# LANGUAGE NumericUnderscores #-}

import           Control.DeepSeq    (deepseq)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List          (find, iterate', uncons)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (fromJust)

import           Debug.Trace

type Case = [Int]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = map read . splitOn ","

display :: Soln -> String
display = unlines . return . show

data State = State { turn :: Int, seed :: [Int], seen :: IntMap Int, curr :: Int }

instance Show State where
  show s = show $ (curr s)

initState :: [Int] -> State
initState is = State { turn = 1, seed = tail is, seen = IntMap.empty, curr = head is }

step :: State -> State
step s = deepseq curr' $ State { turn = turn', seed = seed', seen = seen', curr = curr' }
  where
    turn' = succ (turn s)
    seed' = maybe [] snd $ uncons (seed s)
    seen' = IntMap.insert (curr s) (turn s) (seen s)
    curr' = maybe step' fst $ uncons (seed s)
    step' = maybe 0 ((turn s)-) $ IntMap.lookup (curr s) (seen s)

target :: Int
target = 30_000_000

solve :: Case -> Soln
solve = last . take target . map curr . iterate' step . initState
