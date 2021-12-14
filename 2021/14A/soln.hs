import           Data.Char                    (isUpper)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

type Rule = ((Char, Char), Char)
type Case = (String, [Rule])
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = fst . head . (ReadP.readP_to_S parseCase)

parseCase :: ReadP Case
parseCase = do
  p <- ReadP.many (ReadP.satisfy isUpper)
  rs <- ReadP.many (ReadP.skipSpaces >> parseRule)
  ReadP.skipSpaces
  ReadP.eof
  return (p, rs)

parseRule :: ReadP Rule
parseRule = do
  l <- ReadP.satisfy isUpper
  r <- ReadP.satisfy isUpper
  ReadP.string " -> "
  m <- ReadP.satisfy isUpper
  return ((l, r), m)

showSoln :: Soln -> String
showSoln = unlines . return . show

applyRules :: Map (Char, Char) Char -> String -> String
applyRules rm s | length s < 2 = s
applyRules rm (l:r:s) = l:(Map.findWithDefault '?' (l, r) rm):(applyRules rm (r:s))

solve :: Case -> Soln
solve (p, rs) = (maximum counts) - (minimum counts)
  where
    rm = Map.fromList rs
    p' = (!! 10) $ iterate (applyRules rm) p
    hist = Map.fromListWith (+) $ zip p' $ repeat 1
    counts = Map.elems hist
