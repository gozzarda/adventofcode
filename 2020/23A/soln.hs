import           Data.Char   (isDigit)
import           Data.List   (sort)

import           Debug.Trace

type Case = [Int]
type Soln = [Int]

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map (read . return) . filter isDigit

showSoln :: Soln -> String
showSoln = unlines . return . concat . map show

type State = [Int]

step :: State -> State
step (curr:cups) = pref ++ [dest] ++ cuts ++ suff ++ [curr]
  where
    (cuts, cups') = splitAt 3 cups
    opts = dropWhile (>=curr) $ reverse $ sort $ cups'
    dest = if null opts then maximum cups' else head opts
    (pref, _:suff) = break (==dest) cups'

solve :: Case -> Soln
solve cups = res
  where
    steps = drop 100 $ iterate step cups
    final = head steps
    (_:res) = take (length final) $ dropWhile (/=1) $ cycle final
