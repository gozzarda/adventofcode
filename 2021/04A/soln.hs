import           Data.Function   (on)
import           Data.IntMap     (IntMap)
import qualified Data.IntMap     as IntMap
import           Data.IntSet     (IntSet)
import qualified Data.IntSet     as IntSet
import           Data.List       (minimumBy, transpose)
import           Data.List.Split (chunksOf, splitWhen)

type Board = [[Int]]
type Case = ([Int], [Board])
type Soln = Int

nrows = 5 :: Int
ncols = 5 :: Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase s = (order, boards)
  where
    orderStr : cellStrs = words s
    order = map read $ splitWhen (== ',') orderStr
    cells = map read cellStrs
    boards = chunksOf nrows $ chunksOf ncols $ cells

showSoln :: Soln -> String
showSoln = unlines . return . show

winConditions :: Board -> [[Int]]
winConditions b = b ++ (transpose b)

winConditionTime :: IntMap Int -> [Int] -> Int
winConditionTime ts = maximum . map (\x -> IntMap.findWithDefault maxBound x ts)

winConditionsTime :: IntMap Int -> [[Int]] -> Int
winConditionsTime ts = minimum . map (winConditionTime ts)

winningBoard :: [Int] -> [Board] -> (Board, Int)
winningBoard o bs = minimumBy (compare `on` snd) $ zip bs wts
  where
    ts = IntMap.fromList $ zip o [1..]
    wts = map (winConditionsTime ts . winConditions) bs

solve :: Case -> Soln
solve (o, bs) = (sum $ IntSet.elems remaining) * (o !! (pred t))
  where
    (b, t) = winningBoard o bs
    remaining = IntSet.difference (IntSet.fromList $ concat b) (IntSet.fromList $ take t o)
