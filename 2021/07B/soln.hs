import           Data.List.Split (splitWhen)

type Case = [Int]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map read . splitWhen (== ',')

showSoln :: Soln -> String
showSoln = unlines . return . show

cost :: [Int] -> Int -> Int
cost xs x = sum $ map ((\d -> d * (d + 1) `div` 2) . abs . ((-) x)) xs
-- cost xs x = sum $ map (abs . ((-) x)) xs -- Part A

gradient :: [Int] -> Int -> Int
gradient xs x = cost xs (succ x) - cost xs x

binSearch :: (Int -> Bool) -> Int -> Int -> Int
binSearch f l u | l == u = l
binSearch f l u = let m = (l + u) `div` 2 in if f m then binSearch f l m else binSearch f (succ m) u

solve :: Case -> Soln
solve ds = cost ds $ binSearch ((>= 0) . gradient ds) (minimum ds) (maximum ds)
-- solve ds = minimum $ map (cost ds) [minimum ds .. maximum ds] -- Naive
