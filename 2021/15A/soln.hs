import           Data.Char  (digitToInt)
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (catMaybes)
import           Data.Set   (Set)
import qualified Data.Set   as Set

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

gridToMap :: [[a]] -> Map Coord a
gridToMap = Map.fromList . concat . zipWith (\r -> zipWith (\c x -> ((r, c), x)) [0..]) [0..]

neighbours :: Map Coord a -> Coord -> [(Coord, a)]
neighbours m (r, c) = catMaybes $ map f deltas
  where
    f (dr, dc) = let k = (r + dr, c + dc) in fmap ((,) k) $ Map.lookup k m
    deltas = [(-1, 0), (0, -1), (0, 1), (1, 0)]

-- (Locked-in shortest path lengths, priority queue)
type State = (Map Coord Int, Map Int (Set Coord))

initState :: State
initState = (Map.empty, Map.singleton 0 $ Set.singleton (0, 0))

stepState :: Map Coord Int -> State -> State
stepState wm (spm, pq) = (spm', pq'')
  where
    ((d, us), pq') = Map.deleteFindMin pq
    (spm', us') = Set.foldl f (spm, []) us -- Insert paths into map and find what ones were actually new in one pass
    f (m, ks) k = let (mv, m') = Map.insertLookupWithKey (const const) k d m in (m', maybe (k:ks) (const ks) mv)
    es = filter (flip Map.notMember spm' . fst) $ concat $ map (neighbours wm) us'
    pq'' = foldl (\q (v, w) -> Map.insertWith Set.union (d + w) (Set.singleton v) q) pq' es

haltState :: State -> Bool
haltState = Map.null . snd

statState :: State -> Map Coord Int
statState = fst

solve :: Case -> Soln
solve grid = snd $ Map.findMax $ statState $ last states
  where
    wm = gridToMap grid
    step = stepState wm
    states = takeWhile (not . haltState) $ iterate step initState
