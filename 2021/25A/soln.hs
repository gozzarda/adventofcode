import           Data.List  (partition)
import           Data.Maybe
import           Data.Set   (Set)
import qualified Data.Set   as Set

data Dir = S | E deriving (Eq)
type Case = [[Maybe Dir]]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map (map charToDir) . lines

charToDir :: Char -> Maybe Dir
charToDir 'v' = Just S
charToDir '>' = Just E
charToDir _   = Nothing

showSoln :: Soln -> String
showSoln = unlines . return . show

both :: (a -> b) -> (a, a) -> (b, b)
both f (l, r) = (f l, f r)

type Coord = (Int, Int)
type Bounds = Coord
type State = (Set Coord, Set Coord)

initState :: [[Maybe Dir]] -> State
initState = both (Set.fromList . map fst) . partition ((== S) . snd) . catMaybes . concat . zipWith (\s -> zipWith (\e-> fmap ((,) (s, e))) [0..]) [0..]

nextS :: Bounds -> Coord -> Coord
nextS (ms, _) (cs, ce) = (mod (succ cs) ms, ce)

nextE :: Bounds -> Coord -> Coord
nextE (_, me) (cs, ce) = (cs, mod (succ ce) me)

stepStateE :: Bounds -> State -> State
stepStateE b (ss, es) = (ss, Set.map (\c -> let n = nextE b c in if Set.member n ss || Set.member n es then c else n) es)

stepStateS :: Bounds -> State -> State
stepStateS b (ss, es) = (Set.map (\c -> let n = nextS b c in if Set.member n ss || Set.member n es then c else n) ss, es)

stepState :: Bounds -> State -> State
stepState b = stepStateS b . stepStateE b

solve :: Case -> Soln
solve grid = succ $ length $ takeWhile (uncurry (/=)) $ zip states $ tail states
  where
    bounds = (length grid, maximum $ map length grid)
    states = iterate (stepState bounds) $ initState grid
