type Move = (Int, Int)
type Case = [Move]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readMove . lines

readMove :: String -> Move
readMove s = case dir of
    "forward" -> (dist, 0)
    "up"      -> (0, -dist)
    "down"    -> (0, dist)
  where
    dir : sdist : _ = words s
    dist = read sdist

showSoln :: Soln -> String
showSoln = unlines . return . show

solve :: Case -> Soln
solve ms = x * y
  where
    (dxs, dys) = unzip ms
    x = sum dxs
    y = sum dys
