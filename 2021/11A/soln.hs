import           Data.Char       (digitToInt)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes)

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
    deltas = [ (dr, dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0) ]

threshold = 9 :: Int

handleFlash :: Map Coord Int -> Map Coord Int
handleFlash m = Map.mapWithKey f m
  where
    f k 0 = 0
    f k v | v > threshold = 0
    f k v = (+) v $ length $ filter ((> threshold) . snd) $ neighbours m k

handleFlashes :: Map Coord Int -> Map Coord Int
handleFlashes m = let m' = handleFlash m in if m' == m then m else handleFlashes m'

countFlashes :: Map Coord Int -> Int
countFlashes = length . filter (== 0) . Map.elems

step :: Map Coord Int -> Map Coord Int
step = handleFlashes . Map.map (succ)

solve :: Case -> Soln
solve = sum . map countFlashes . take 100 . tail . iterate step . gridToMap
