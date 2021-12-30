import           Data.List (tails)

type Case = [Int]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = (map read) . lines

display :: Soln -> String
display = unlines . return . show

options :: Int -> [Int] -> [[Int]]
options range vals = map (f . (take range)) $ tails vals
  where
    f vs = [x + y | (x:ys) <- tails vs, y <- ys, x /= y]

findInvalid :: [Int] -> Int
findInvalid vals = fst $ head $ filter (not . (uncurry elem)) $ zip (drop 25 vals) (options 25 vals)

cumulative :: [Int] -> [Int]
cumulative = scanl1 (+)

findRangeSum :: Int -> [Int] -> [Int]
findRangeSum target vals = f 0 vals vals
  where
    f total left right = case (compare total target) of
      EQ -> take ((length left) - (length right)) left
      LT -> f (total + (head right)) left (tail right)
      GT -> f (total - (head left)) (tail left) right

solve :: Case -> Soln
solve vals = (\xs -> (minimum xs) + (maximum xs)) $ findRangeSum (findInvalid vals) vals
