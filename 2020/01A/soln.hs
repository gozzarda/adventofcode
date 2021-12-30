import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

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

solve :: Case -> Soln
solve vs = (target - c) * c
  where
    target = 2020
    test v = IntSet.member (target - v) $ IntSet.fromList vs
    c = head $ filter test vs
