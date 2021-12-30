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

solve :: Case -> Soln
solve tss = length $ filter id hs
  where
    n = length $ head tss
    is = map (flip mod n) [0, 3..]
    hs = zipWith (!!) tss is
