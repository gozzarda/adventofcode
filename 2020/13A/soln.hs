import           Data.List.Split (splitOn)
import           Data.Maybe      (catMaybes)
import           Text.Read       (readMaybe)

type Case = (Int, [Maybe Int])
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse s = (t, ms)
  where
    (ts:mss:_) = lines s
    t = read ts
    ms = map readMaybe $ splitOn "," mss

display :: Soln -> String
display = unlines . return . show

solve :: Case -> Soln
solve (t, ms) = w * b
  where
    bs = catMaybes ms
    ws = map ((-t) `mod`) bs
    (w, b) = minimum $ zip ws bs
