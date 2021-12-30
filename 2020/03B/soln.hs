type Case = [[Bool]]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = map (map (=='#')) . lines

display :: Soln -> String
display = unlines . singleton . show

singleton :: a -> [a]
singleton x = [x]

slopes :: [(Int, Int)]
slopes = [
  (1, 1),
  (3, 1),
  (5, 1),
  (7, 1),
  (1, 2)]

solve :: Case -> Soln
solve tss = product $ map (num_trees tss) slopes

num_trees :: Case -> (Int, Int) -> Soln
num_trees tss (r, d) = length $ filter id hs
  where
    n = length $ head tss
    is = map (`mod` n) [0, r..]
    tss' = every_nth d tss
    hs = zipWith (!!) tss' is

every_nth :: Int -> [a] -> [a]
every_nth _ []       = []
every_nth n xs@(x:_) = x:(every_nth n $ drop n xs)
