import           Data.Char                    (isDigit)
import           Data.List                    (splitAt)
import           Data.Maybe
import           Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

type Case = (Int, Int)
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = fst . fromMaybe (error "Failed to parse case") . listToMaybe . ReadP.readP_to_S parseCase

showSoln :: Soln -> String
showSoln = unlines . return . show

parseNat :: ReadP Int
parseNat = read <$> (ReadP.many1 $ ReadP.satisfy isDigit)

parseCase :: ReadP Case
parseCase = do
  p1 <- ReadP.string "Player 1 starting position: " >> parseNat
  ReadP.skipSpaces
  p2 <- ReadP.string "Player 2 starting position: " >> parseNat
  ReadP.skipSpaces
  ReadP.eof
  return (p1, p2)

data Player = Player { pos :: Int, score :: Int } deriving (Show)
data State = State { turn :: Int, die :: Int, p1 :: Player, p2 :: Player } deriving (Show)
boardSize = 10 :: Int
dieSize = 100 :: Int
rollsPerTurn = 3 :: Int
scoreLimit = 1000 :: Int

initPlayer :: Int -> Player
initPlayer p = Player { pos = p, score = 0 }

movePlayer :: Int -> Player -> Player
movePlayer m p = let pos' = succ $ flip mod boardSize $ (+ m) $ pred $ pos p in Player { pos = pos', score = score p + pos' }

initState :: Case -> State
initState (p1, p2) = State { turn = 0, die = 1, p1 = initPlayer p1, p2 = initPlayer p2 }

stepState :: State -> State
stepState s = if even $ turn s then s' { p1 = movePlayer move $ p1 s } else s' { p2 = movePlayer move $ p2 s }
  where
    (rolls, die':_) = splitAt rollsPerTurn $ iterate (succ . flip mod dieSize) $ die s
    move = sum rolls
    s' = s { turn = succ $ turn s, die = die' }

haltState :: State -> Bool
haltState s = (max (score $ p1 s) (score $ p2 s)) >= scoreLimit

statState :: State -> Int
statState s = (rollsPerTurn * turn s) * (min (score $ p1 s) (score $ p2 s))

solve :: Case -> Soln
solve = statState . head . dropWhile (not . haltState) . iterate stepState . initState
