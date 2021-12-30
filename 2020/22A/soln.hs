import           Data.Foldable   (toList)
import           Data.List.Split (splitOn)
import           Data.Sequence   (Seq, ViewL (..), (<|), (><), (|>))
import qualified Data.Sequence   as Seq

import           Debug.Trace

type Case = ([Int], [Int])
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase s = (ls, rs)
  where
    (ls:rs:_) = map ((map read) . tail) $ splitOn [""] $ lines s

showSoln :: Soln -> String
showSoln = unlines . return . show

type State = (Seq Int, Seq Int)

initState :: Case -> State
initState (ls, rs) = (Seq.fromList ls, Seq.fromList rs)

step :: State -> State
step (ls, rs) = if l < r
    then (ls', rs' |> r |> l)
    else (ls' |> l |> r, rs')
  where
    (l :< ls') = Seq.viewl ls
    (r :< rs') = Seq.viewl rs

terminal :: State -> Bool
terminal (ls, rs) = Seq.null ls || Seq.null rs

winnerScore :: State -> Int
winnerScore (ls, rs) = sum $ zipWith (*) [1..] $ reverse $ toList $ ls >< rs

solve :: Case -> Soln
solve = winnerScore . head . dropWhile (not . terminal) . iterate step . initState
