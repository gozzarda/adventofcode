import           Data.Char                    (isDigit)
import           Data.Maybe                   (fromMaybe, listToMaybe)
import           Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

type Range = (Int, Int)
type Case = (Range, Range)
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

solve :: Case -> Soln
solve ((xl, xu), (yl, yu)) = div (vy * vy + vy) 2
  where
    vy = max yu (negate $ succ yl)
