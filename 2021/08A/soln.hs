import           Data.List.Split (splitWhen)

type Row = ([String], [String])
type Case = [Row]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readRow . lines

readRow :: String -> Row
readRow = (\[us, os] -> (us, os)) . (splitWhen (== "|")) . words

showSoln :: Soln -> String
showSoln = unlines . return . show

solve :: Case -> Soln
solve = length . filter f . concat . snd . unzip
  where
    f s = let l = length s in l == 2 || l == 3 || l == 4 || l == 7
