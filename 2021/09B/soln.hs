import           Data.Char  (digitToInt)
import           Data.List  (sortOn)
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import           Data.Ord   (Down (..))

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

type Coord = (Int, Int)
type Height = Int

gridToMap :: [[a]] -> Map Coord a
gridToMap = Map.fromList . concat . zipWith (\r -> zipWith (\c x -> ((r, c), x)) [0..]) [0..]

neighbours :: Map Coord a -> Coord -> [(Coord, a)]
neighbours m (r, c) = catMaybes $ map f deltas
  where
    f (dr, dc) = let k = (r + dr, c + dc) in fmap ((,) k) $ Map.lookup k m
    deltas = [(-1, 0), (0, -1), (0, 1), (1, 0)]

basinRoots :: Map Coord Height -> Map Coord Coord
basinRoots m = roots
  where
    roots = Map.mapWithKey root $ Map.filter (< 9) m
    root k v = fromMaybe k $ (flip Map.lookup roots) =<< (parent k v)
    parent k v = listToMaybe $ map fst $ filter ((< v) . snd) $ neighbours m k

basinSizes :: Map Coord Height -> Map Coord Int
basinSizes m = Map.fromListWith (+) $ zip (Map.elems $ basinRoots m) $ repeat 1

solve :: Case -> Soln
solve = product . take 3 . sortOn Down . Map.elems . basinSizes . gridToMap
