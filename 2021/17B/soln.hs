import           Data.Char                    (isDigit)
import           Data.Maybe                   (fromMaybe, listToMaybe)
import           Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

type Range = (Int, Int)
type Target = (Range, Range)
type Case = Target
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = fst . fromMaybe (error "Failed to parse case") . listToMaybe . ReadP.readP_to_S parseCase

showSoln :: Soln -> String
showSoln = unlines . return . show

parseInt :: ReadP Int
parseInt = do
  (s, _) <- ReadP.gather $ (ReadP.optional $ ReadP.char '-') >> (ReadP.many1 $ ReadP.satisfy isDigit)
  return $ read s

parseRange :: ReadP Range
parseRange = do
  lwr <- parseInt
  ReadP.string ".."
  upr <- parseInt
  return (lwr, upr)

parseCase :: ReadP Case
parseCase = do
  ReadP.string "target area: x="
  xr <- parseRange
  ReadP.string ", y="
  yr <- parseRange
  ReadP.skipSpaces
  ReadP.eof
  return (xr, yr)

-- Naive brute force solution
-- See notes.hs for notes on a potential much more efficient solution

type Position = (Int, Int)
type Velocity = (Int, Int)
type State = (Position, Velocity)

initState :: Velocity -> State
initState v = ((0, 0), v)

stepState :: State -> State
stepState ((px, py), (vx, vy)) = (p', v')
  where
    p' = (px + vx, py + vy)
    v' = (vx - signum vx, pred vy)

haltState :: Target -> State -> Bool
haltState ((xl, xu), (yl, yu)) ((px, py), (vx, vy)) = py < yl && vy <= 0 || vx == 0 && (px < xl || xu < px)

statState :: Target -> State -> Bool
statState ((xl, xu), (yl, yu)) ((px, py), _) = xl <= px && px <= xu && yl <= py && py <= yu

hitsTarget :: Target -> Velocity -> Bool
hitsTarget t = any (statState t) . takeWhile (not . haltState t) . iterate stepState . initState

solve :: Case -> Soln
solve t = length $ filter (hitsTarget t) vs
  where
    ((xl, xu), (yl, yu)) = t
    vxb = max (abs xl) (abs xu)
    vyb = max (abs yl) (abs yu)
    vs = [ (vx, vy) | vx <- [-vxb..vxb], vy <- [-vyb..vyb] ]
