import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.List   (tails)

type Case = [Int]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = map read . lines

display :: Soln -> String
display = unlines . singleton . show

singleton :: a -> [a]
singleton x = [x]

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

solve :: Case -> Soln
solve vs = (target - x - y) * x * y
  where
    target = 2020
    test (u, v) = IntSet.member (target - u - v) $ IntSet.fromList vs
    (x, y) = head $ filter test $ pairs vs
