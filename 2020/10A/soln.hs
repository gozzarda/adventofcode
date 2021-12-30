import           Data.List (sort)

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

solve :: Case -> Soln
solve vals = (length $ filter (==1) diffs) * (length $ filter (==3) diffs)
  where
    levels = [0] ++ (sort vals) ++ [(maximum vals) + 3]
    diffs = zipWith (-) (tail levels) levels
