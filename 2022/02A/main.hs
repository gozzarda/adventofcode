module Main where

data LToken = A | B | C deriving (Read)

data RToken = X | Y | Z deriving (Read)

type Case = [(LToken, RToken)]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map (\s -> let [l, r] = words s in (read l, read r)) . lines

showSoln :: Soln -> String
showSoln = unlines . return . show

data Move = Rock | Paper | Scissors deriving (Eq)

moveSucc :: Move -> Move
moveSucc Rock = Paper
moveSucc Paper = Scissors
moveSucc Scissors = Rock

movePred :: Move -> Move
movePred Rock = Scissors
movePred Paper = Rock
movePred Scissors = Paper

moveScore :: Move -> Int
moveScore Rock = 1
moveScore Paper = 2
moveScore Scissors = 3

data Result = Lose | Win | Draw

gameResult :: Move -> Move -> Result
gameResult o u | o == u = Draw
gameResult o u | o == moveSucc u = Lose
gameResult _ _ = Win

resultScore :: Result -> Int
resultScore Lose = 0
resultScore Draw = 3
resultScore Win = 6

type Game = (Move, Move)

toGame :: (LToken, RToken) -> Game
toGame (l, r) = (fromLToken l, fromRToken r)

fromLToken :: LToken -> Move
fromLToken A = Rock
fromLToken B = Paper
fromLToken C = Scissors

fromRToken :: RToken -> Move
fromRToken X = Rock
fromRToken Y = Paper
fromRToken Z = Scissors

gameScore :: Game -> Int
gameScore (o, u) = moveScore u + resultScore r
  where
    r = gameResult o u

solve :: Case -> Soln
solve = sum . map (gameScore . toGame)
