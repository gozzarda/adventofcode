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

type State = Map (Int, Int, Int) Bool

initState :: Case -> State
initState bss = Map.fromList $ concat $ zipWith (\x -> zipWith (\y -> (,) (x, y, 0)) [0..]) [0..] bss

rules :: Bool -> Int -> Bool
rules True n  = n == 2 || n == 3
rules False n = n == 3

deltas :: [(Int, Int, Int)]
deltas = [(dx, dy, dz) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dz <- [-1, 0, 1], (dx, dy, dz) /= (0, 0, 0)]

add3 :: (Num a) => (a, a, a) -> (a, a, a) -> (a, a, a)
add3 (xl, yl, zl) (xr, yr, zr) = (xl + xr, yl + yr, zl + zr)

neighbours :: Map (Int, Int, Int) Bool -> Map (Int, Int, Int) Int
neighbours bm = nm
  where
    im = Map.map (bool 0 1) bm
    nms = map (\d -> Map.mapKeys (add3 d) im) deltas
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
