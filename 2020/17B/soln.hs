import           Data.Bool (bool)
import           Data.Map  (Map)
import qualified Data.Map  as Map

type Case = [[Bool]]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = map (map (=='#')) . lines

display :: Soln -> String
display = unlines . return . show

type Coord = (Int, Int, Int, Int)
type State = Map Coord Bool

initState :: Case -> State
initState bss = Map.fromList $ concat $ zipWith (\x -> zipWith (\y -> (,) (x, y, 0, 0)) [0..]) [0..] bss

rules :: Bool -> Int -> Bool
rules True n  = n == 2 || n == 3
rules False n = n == 3

deltas :: [Coord]
deltas = [(dx, dy, dz, dw) | dx <- ads, dy <- ads, dz <- ads, dw <- ads, (dx, dy, dz, dw) /= (0, 0, 0, 0)]
  where
    ads = [-1, 0, 1]

addCoords :: Coord -> Coord -> Coord
addCoords (xl, yl, zl, wl) (xr, yr, zr, wr) = (xl + xr, yl + yr, zl + zr, wl + wr)

neighbours :: Map Coord Bool -> Map Coord Int
neighbours bm = nm
  where
    im = Map.map (bool 0 1) bm
    nms = map (\d -> Map.mapKeys (addCoords d) im) deltas
    nm = Map.unionsWith (+) nms

step :: State -> State
step bm = Map.map (uncurry rules) bnm
  where
    nm = neighbours bm
    bm' = Map.union bm $ Map.map (const False) nm
    bnm = Map.intersectionWith (,) bm' nm

solve :: Case -> Soln
solve bss = n
  where
    final = iterate step (initState bss) !! 6
    n = Map.size $ Map.filter id final
