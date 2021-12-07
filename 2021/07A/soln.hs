import           Data.List       (sort)
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

-- median, biased up if even
median :: Ord a => [a] -> a
median xs = head $ drop (length xs `div` 2) $ sort xs

solve :: Case -> Soln
solve ds = sum $ map (abs . (subtract $ median ds)) ds
