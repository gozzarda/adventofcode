import           Data.List (transpose)

type Case = [[Bool]]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map (map (== '1')) . lines

showSoln :: Soln -> String
showSoln = unlines . return . show

boolsToInt :: [Bool] -> Int
boolsToInt = sum . zipWith (*) (iterate (2 *) 1) . map fromEnum . reverse

solve :: Case -> Soln
solve bss = g * e
  where
    n = length bss
    nts = map (length . filter id) $ transpose bss
    nfs = map ((-) n) nts
    gbs = zipWith (>) nts nfs
    ebs = map not gbs
    g = boolsToInt gbs
    e = boolsToInt ebs
