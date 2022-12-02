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

resultMove :: Result -> Move -> Move
resultMove Lose = movePred
resultMove Draw = id
resultMove Win = moveSucc

resultScore :: Result -> Int
resultScore Lose = 0
resultScore Draw = 3
resultScore Win = 6

type Game = (Move, Result)

toGame :: (LToken, RToken) -> Game
toGame (l, r) = (fromLToken l, fromRToken r)

fromLToken :: LToken -> Move
fromLToken A = Rock
fromLToken B = Paper
fromLToken C = Scissors

fromRToken :: RToken -> Result
fromRToken X = Lose
fromRToken Y = Draw
fromRToken Z = Win

gameScore :: Game -> Int
gameScore (o, r) = moveScore u + resultScore r
  where
    u = resultMove r o

solve :: Case -> Soln
solve = sum . map (gameScore . toGame)
