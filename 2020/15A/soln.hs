import           Data.IntMap     (IntMap)
import qualified Data.IntMap     as IntMap
import           Data.List       (find, uncons)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromJust)

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

initState :: [Int] -> State
initState is = State { turn = 1, seed = tail is, seen = IntMap.empty, curr = head is }

step :: State -> State
step s = State { turn = turn', seed = seed', seen = seen', curr = curr' }
  where
    turn' = succ (turn s)
    seed' = maybe [] snd $ uncons (seed s)
    seen' = IntMap.insert (curr s) (turn s) (seen s)
    curr' = maybe step' fst $ uncons (seed s)
    step' = maybe 0 ((turn s)-) $ IntMap.lookup (curr s) (seen s)

solve :: Case -> Soln
solve = last . traceShowId . take 2020 . map curr . iterate step . initState
