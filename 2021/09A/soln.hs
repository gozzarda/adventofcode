import           Data.Char  (digitToInt)
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (catMaybes)

type Case = [[Int]]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map (map digitToInt) . words

showSoln :: Soln -> String
showSoln = unlines . return . show

gridToMap :: [[a]] -> Map (Int, Int) a
gridToMap = Map.fromList . concat . zipWith (\r -> zipWith (\c x -> ((r, c), x)) [0..]) [0..]

neighbours :: Map (Int, Int) a -> (Int, Int) -> [a]
neighbours m (r, c) = catMaybes $ map ((flip Map.lookup m) . (\(dr, dc) -> (r + dr, c + dc))) deltas
  where
    deltas = [(-1, 0), (0, -1), (0, 1), (1, 0)]

solve :: Case -> Soln
solve grid = sum $ map succ ls
  where
    m = gridToMap grid
    ls = Map.elems $ Map.filterWithKey (\k v -> all (> v) $ neighbours m k) m
