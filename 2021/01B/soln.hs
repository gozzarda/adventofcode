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

windowSums :: (Num a) => Int -> [a] -> [a]
windowSums n xs = scanl (+) (sum pref) diffs
  where
    (pref, suff) = splitAt n xs
    diffs = zipWith (-) suff xs

solve :: Case -> Soln
solve = length . filter (uncurry (<)) . pairs . windowSums 3
  where
    pairs xs = zip xs $ tail xs
