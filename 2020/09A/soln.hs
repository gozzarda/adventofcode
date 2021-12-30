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

--pairSumOptions :: Int -> [Int] -> [[Int]]
--pairSumOptions range vals = map ((\(v:vs) -> map (+v) vs) . (take range)) $ init $ tails vals

--optionsDiffs :: Int -> [Int] -> [([Int], [Int])]
--optionsDiffs range vals = zip (adds ++ pad) (pad ++ rems)
--  where
--    pad = replicate range []
--    adds = reverse $ pairSumOptions range $ reverse vals
--    rems = pairSumOptions range vals

--options :: Int -> [Int] -> [[Int]]
--options range vals = scanl (\opts (adds, rems) -> adds ++ (opts \\ rems)) [] $ optionsDiffs range vals

options :: Int -> [Int] -> [[Int]]
options range vals = map (f . (take range)) $ tails vals
  where
    f vs = [x + y | (x:ys) <- tails vs, y <- ys, x /= y]

solve :: Case -> Soln
solve vals = fst $ head $ filter (not . (uncurry elem)) $ zip (drop 25 vals) (options 25 vals)
