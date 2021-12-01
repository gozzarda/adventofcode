type Case = [Int]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map read . words

showSoln :: Soln -> String
showSoln = unlines . return . show

solve :: Case -> Soln
solve = length . filter (uncurry (<)) . pairs
  where
    pairs xs = zip xs $ tail xs