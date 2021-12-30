import           Data.Map    (Map)
import qualified Data.Map    as Map

import           Debug.Trace

data Dir = E | SE | SW | W | NW | NE deriving (Show)
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

dirCoord :: Dir -> Coord
dirCoord E  = (1, 0)
dirCoord SE = (1, -1)
dirCoord SW = (0, -1)
dirCoord W  = (-1, 0)
dirCoord NW = (-1, 1)
dirCoord NE = (0, 1)

sumCoord :: Coord -> Coord -> Coord
sumCoord (fl, sl) (fr, sr) = (fl + fr, sl + sr)

dirsToCoord :: [Dir] -> Coord
dirsToCoord = foldr1 sumCoord . map dirCoord

solve :: Case -> Soln
solve dss = Map.size $ Map.filter odd cm
  where
    cm = Map.fromListWith (+) $ zip (map dirsToCoord dss) (repeat 1)
