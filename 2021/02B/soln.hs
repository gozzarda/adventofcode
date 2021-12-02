data Op = Forward Int | Up Int | Down Int
type Case = [Op]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readOp . lines

readOp :: String -> Op
readOp s = case op of
    "forward" -> Forward val
    "up"      -> Up val
    "down"    -> Down val
  where
    op : sval : _ = words s
    val = read sval

showSoln :: Soln -> String
showSoln = unlines . return . show

data State = State { hpos :: Int, depth :: Int, aim :: Int }

initState :: State
initState = State { hpos = 0, depth = 0, aim = 0 }

updateState :: State -> Op -> State
updateState state (Forward val) = state { hpos = (hpos state) + val, depth = (depth state) + (aim state) * val }
updateState state (Up val) = state { aim = (aim state) - val }
updateState state (Down val) = state { aim = (aim state) + val }

solve :: Case -> Soln
solve ops = (hpos finalState) * (depth finalState)
  where
    finalState = foldl updateState initState ops
