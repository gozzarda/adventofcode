import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Set    (Set)
import qualified Data.Set    as Set

import           Debug.Trace

data Dir = E | SE | SW | W | NW | NE deriving (Show, Eq, Ord, Enum, Bounded)
type Case = [[Dir]]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readDirs . lines

readDirs :: String -> [Dir]
readDirs []          = []
readDirs ('e':s)     = E : (readDirs s)
readDirs ('s':'e':s) = SE : (readDirs s)
readDirs ('s':'w':s) = SW : (readDirs s)
readDirs ('w':s)     = W : (readDirs s)
readDirs ('n':'w':s) = NW : (readDirs s)
readDirs ('n':'e':s) = NE : (readDirs s)

showSoln :: Soln -> String
showSoln = unlines . return . show

type Coord = (Int, Int)

sumCoord :: Coord -> Coord -> Coord
sumCoord (fl, sl) (fr, sr) = (fl + fr, sl + sr)

dirCoord :: Dir -> Coord
dirCoord E  = (1, 0)
dirCoord SE = (1, -1)
dirCoord SW = (0, -1)
dirCoord W  = (-1, 0)
dirCoord NW = (-1, 1)
dirCoord NE = (0, 1)

dirsCoord :: [Dir] -> Coord
dirsCoord = foldr1 sumCoord . map dirCoord

type State = Set Coord

initState :: Case -> State
initState dss = Map.keysSet $ Map.filter odd $ cm
  where
    cm = Map.fromListWith (+) $ zip (map dirsCoord dss) (repeat 1)

rule :: Bool -> Int -> Bool
rule True n  = n <= 2
rule False n = n == 2

step :: State -> State
step state = state'
  where
    ns' = Map.fromSet (const 1) state
    nss = map (\c -> Map.mapKeys (sumCoord c) ns') $ map dirCoord [minBound..maxBound]
    ns = Map.unionsWith (+) nss
    bs = Map.union (Map.fromSet (const True) state) (Map.map (const False) ns)
    bns = Map.intersectionWith (,) bs ns
    state' = Map.keysSet $ Map.filter (uncurry rule) bns

solve :: Case -> Soln
solve = Set.size . head . drop 100 . iterate step . initState
