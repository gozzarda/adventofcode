import           Data.Char                    (isDigit)
import           Data.List                    (foldl')
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

type Coord = (Int, Int)
data Fold = H Int | V Int deriving (Show)
type Case = ([Coord], [Fold])
type Soln = Set Coord

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = fst . head . (ReadP.readP_to_S parseCase)

parseCase :: ReadP Case
parseCase = do
  cs <- ReadP.many (ReadP.skipSpaces >> parseCoord)
  fs <- ReadP.many (ReadP.skipSpaces >> parseFold)
  ReadP.skipSpaces
  ReadP.eof
  return (cs, fs)

parseCoord :: ReadP Coord
parseCoord = do
  x <- parseNat
  ReadP.char ','
  y <- parseNat
  return (x, y)

parseFold :: ReadP Fold
parseFold = do
  ReadP.string "fold along "
  ReadP.choice
    [ ReadP.string "y=" >> return H
    , ReadP.string "x=" >> return V
    ] <*> parseNat

parseNat :: ReadP Int
parseNat = read <$> (ReadP.many1 $ ReadP.satisfy isDigit)

showSoln :: Soln -> String
showSoln s = unlines [ [ if Set.member (x, y) s then '#' else '.' | x <- [minx..maxx] ] | y <- [miny..maxy] ]
  where
    minx = minimum $ map fst $ Set.elems s
    maxx = maximum $ map fst $ Set.elems s
    miny = minimum $ map snd $ Set.elems s
    maxy = maximum $ map snd $ Set.elems s

foldCoord :: Fold -> Coord -> Coord
foldCoord (H f) (x, y) = (x, f - (abs $ y - f))
foldCoord (V f) (x, y) = (f - (abs $ x - f), y)

foldCoords :: Set Coord -> Fold -> Set Coord
foldCoords s f = Set.map (foldCoord f) s

solve :: Case -> Soln
solve (cs, fs) = foldl' foldCoords (Set.fromList cs) fs
