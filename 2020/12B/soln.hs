import           Debug.Trace

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

data State = State { she :: Int, shn :: Int, wpe :: Int, wpn :: Int } deriving (Show)

initState :: State
initState = State { she = 0, shn = 0, wpe = 10, wpn = 1 }

moveN :: State -> Int -> State
moveN s d = s { wpn = (wpn s) + d }

moveS :: State -> Int -> State
moveS s d = s { wpn = (wpn s) - d }

moveE :: State -> Int -> State
moveE s d = s { wpe = (wpe s) + d }

moveW :: State -> Int -> State
moveW s d = s { wpe = (wpe s) - d }

turnL :: State -> Int -> State
turnL s a = (iterate stepL s) !! steps
  where
    stepL s' = s' { wpe = -(wpn s'), wpn = (wpe s') }
    steps = a `div` 90 `mod` 4

turnR :: State -> Int -> State
turnR s a = (iterate stepR s) !! steps
  where
    stepR s' = s' { wpe = (wpn s'), wpn = -(wpe s') }
    steps = a `div` 90 `mod` 4

stepF :: State -> Int -> State
stepF s n = s { she = (she s) + n * (wpe s), shn = (shn s) + n * (wpn s) }

step :: State -> Inst -> State
step s (N d) = moveN s d
step s (S d) = moveS s d
step s (E d) = moveE s d
step s (W d) = moveW s d
step s (L a) = turnL s a
step s (R a) = turnR s a
step s (F n) = stepF s n

manhattanDist :: State -> Int
manhattanDist s = (abs $ she s) + (abs $ shn s)

solve :: Case -> Soln
-- solve = manhattanDist . foldl step initState
solve = manhattanDist . foldl (\l r -> traceShowId $ step l r) initState
