module Main where

data Oper = NoOp | AddX Int

type Case = [Oper]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readOper . lines

readOper :: String -> Oper
readOper s = case words s of
  ["noop"] -> NoOp
  ["addx", vs] -> AddX $ read vs

showSoln :: Soln -> String
showSoln = unlines . return . show

type State = [Int]

initState :: State
initState = [1]

stepState :: State -> Oper -> State
stepState xs@(x : _) NoOp = x : xs
stepState xs@(x : _) (AddX v) = (x + v) : x : xs

statState :: State -> Soln
statState = sum . map (uncurry (*)) . filter ((== 20) . (`mod` 40) . fst) . zip [1 ..] . reverse . tail

solve :: Case -> Soln
solve = statState . foldl stepState initState
