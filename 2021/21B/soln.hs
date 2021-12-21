import           Data.Char                    (isDigit)
import           Data.List                    (splitAt)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.Tuple                   (swap)
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

both :: (a -> b) -> (a, a) -> (b, b)
both f (l, r) = (f l, f r)

allRolls :: Int -> Int -> [[Int]]
allRolls 0 _ = [[]]
allRolls n d = [ r:rs | r <- [1..d], rs <- allRolls (pred n) d ]

diestribution :: Int -> Int -> Map Int Int
diestribution n d = Map.fromListWith (+) $ zip (map sum $ allRolls n d) $ repeat 1

data Player = Player { pos :: Int, score :: Int } deriving (Ord,Eq,Show)
type State = (Player, Player)
type Result = (Int, Int)
boardSize = 10 :: Int
dieSize = 3 :: Int
rollsPerTurn = 3 :: Int
scoreLimit = 21 :: Int

initPlayer :: Int -> Player
initPlayer p = Player { pos = p, score = 0 }

movePlayer :: Int -> Player -> Player
movePlayer m p = let pos' = succ $ flip mod boardSize $ (+ m) $ pred $ pos p in Player { pos = pos', score = score p + pos' }

enumPlayer :: [Player]
enumPlayer = [ Player { pos = pos, score = score } | pos <- [1..boardSize], score <- [0..pred scoreLimit] ]

initState :: Case -> State
initState (p1, p2) = (initPlayer p1, initPlayer p2)

enumState :: [State]
enumState = [ (curr, next) | curr <- enumPlayer, next <- enumPlayer ]

dist :: [(Int, Int)]
dist = Map.assocs $ diestribution rollsPerTurn dieSize

dpt :: Map State Result
dpt = Map.mapWithKey (\s _ -> dpf s) dps
  where
    dps = Map.fromList $ zip enumState $ repeat ()
    dpf (curr, next) = both sum $ unzip subs
      where
        subs = map (\(m, n) -> both (* n) $ swap $ Map.findWithDefault (0, 1) (next, movePlayer m curr) dpt) dist

stat :: Result -> Int
stat (cw, nw) = max cw nw

solve :: Case -> Soln
solve = stat . (dpt Map.!) . initState
