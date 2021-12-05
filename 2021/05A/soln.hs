import           Data.List.Split (splitWhen)
import           Data.Map        (Map)
import qualified Data.Map        as Map

type Point = (Int, Int)
type Segment = (Point, Point)
type Case = [Segment]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readSegment . lines

readSegment :: String -> Segment
readSegment s = let u:_:v:_ = words s in (readPoint u, readPoint v)

readPoint :: String -> Point
readPoint s = let x:y:_ = map read $ splitWhen (== ',') s in (x, y)

showSoln :: Soln -> String
showSoln = unlines . return . show

pointRange :: Point -> Point -> [Point]
pointRange (ux, uy) (vx, vy) = take (succ cd) $ zip xs ys
  where
    rx = (vx - ux)
    ry = (vy - uy)
    cd = gcd rx ry
    dx = div rx cd
    dy = div ry cd
    xs = iterate (+ dx) ux
    ys = iterate (+ dy) uy

segmentCoverage :: Segment -> Map Point Int
segmentCoverage (u, v) | fst u == fst v || snd u == snd v = Map.fromList $ zip (pointRange u v) $ repeat 1
segmentCoverage _ = Map.empty

-- Just make a heatmap... Scanline approach would be way faster but I can't be bothered right now
solve :: Case -> Soln
solve = Map.size . Map.filter (> 1) . Map.unionsWith (+) . map segmentCoverage
