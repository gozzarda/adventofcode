import           Data.Function   (on)
import           Data.List       (groupBy, partition, sortOn)
import           Data.List.Split (splitWhen)
import qualified Data.Map        as Map
import qualified Data.Set        as Set

type Row = ([String], [String])
type Case = [Row]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readRow . lines

readRow :: String -> Row
readRow = (\[us, os] -> (us, os)) . (splitWhen (== "|")) . words

showSoln :: Soln -> String
showSoln = unlines . return . show

groupOn :: (Eq a, Ord a) => (b -> a) -> [b] -> [[b]]
groupOn f = groupBy ((==) `on` f) . sortOn f

solveRow :: Row -> [Int]
solveRow (us, os) = map (vals Map.!) $ map Set.fromList os
  where
    [[s1], [s7], [s4], l235, l069, [s8]] = groupOn Set.size $ map Set.fromList us
    ([s9], l06) = partition (s4 `Set.isSubsetOf`) l069
    (l35, [s2]) = partition (`Set.isSubsetOf` s9) l235
    ([s5], [s3]) = partition ((== 3) . Set.size . (Set.intersection s2)) l35
    ([s6], [s0]) = partition (s5 `Set.isSubsetOf`) l06
    vals = Map.fromList $ zip [s0, s1, s2, s3, s4, s5, s6, s7, s8, s9] [0..]

digitsToInt :: [Int] -> Int
digitsToInt = sum . zipWith (*) (iterate (* 10) 1) . reverse

solve :: Case -> Soln
solve = sum . map (digitsToInt . solveRow)
