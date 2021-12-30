import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (catMaybes)

type Case = [[Char]]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = lines

display :: Soln -> String
display = unlines . return . show

type Grid = Map (Int, Int) Char

toGrid :: Case -> Grid
toGrid vss = Map.fromList [((r, c), v) | (r, vs) <- zip [0..] vss, (c, v) <- zip [0..] vs]

neighbours :: Grid -> (Int, Int) -> [Char]
neighbours grid (r, c) = catMaybes $ map (flip Map.lookup grid) keys
  where
    keys = [(r', c') | r' <- map (r+) [-1..1], c' <- map (c+) [-1..1], (r', c') /= (r, c)]

occupied :: Grid -> (Int, Int) -> Int
occupied grid key = length $ filter (=='#') $ neighbours grid key

step :: Grid -> Grid
step grid = Map.mapWithKey update grid
  where
    update _ '.' = '.'
    update k 'L' = if occupied grid k == 0 then '#' else 'L'
    update k '#' = if occupied grid k >= 4 then 'L' else '#'

solve :: Case -> Soln
solve vss = taken
  where
    grid = toGrid vss
    steps = iterate step grid
    final = fst $ head $ filter (uncurry (==)) $ zip steps $ tail steps
    taken = Map.size $ Map.filter (=='#') final
