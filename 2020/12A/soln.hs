data Inst = N Int | S Int | E Int | W Int | L Int | R Int | F Int deriving (Read)
type Case = [Inst]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = map parseInst . lines

parseInst :: String -> Inst
parseInst (ic:as) = read $ ic : ' ' : as

display :: Soln -> String
display = unlines . return . show

data State = State { row :: Int, col :: Int, ang :: Int }

initState :: State
initState = State { row = 0, col = 0, ang = 0 }

moveN :: State -> Int -> State
moveN s d = s { row = (row s) - d }

moveS :: State -> Int -> State
moveS s d = s { row = (row s) + d }

moveE :: State -> Int -> State
moveE s d = s { col = (col s) + d }

moveW :: State -> Int -> State
moveW s d = s { col = (col s) - d }

turnL :: State -> Int -> State
turnL s a = s { ang = (ang s) + a }

turnR :: State -> Int -> State
turnR s a = s { ang = (ang s) - a }

step :: State -> Inst -> State
step s (N d) = moveN s d
step s (S d) = moveS s d
step s (E d) = moveE s d
step s (W d) = moveW s d
step s (L a) = turnL s a
step s (R a) = turnR s a
step s (F d) = (dirs !! diri) s d
  where
    diri = (ang s) `div` 90 `mod` 4
    dirs = [moveE, moveN, moveW, moveS]

manhattanDist :: State -> Int
manhattanDist s = (abs $ row s) + (abs $ col s)

solve :: Case -> Soln
solve = manhattanDist . foldl step initState
