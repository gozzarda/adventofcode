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

applyRules :: Map (Char, Char) Char -> Map (Char, Char) Int -> Map (Char, Char) Int
applyRules m ph = Map.unionWith (+) lh rh
  where
    lh = Map.mapKeysWith (+) (\(l, r) -> (l, Map.findWithDefault '?' (l, r) m)) ph
    rh = Map.mapKeysWith (+) (\(l, r) -> (Map.findWithDefault '?' (l, r) m, r)) ph

solve :: Case -> Soln
solve (s, rs) = (maximum counts) - (minimum counts)
  where
    m = Map.fromList rs
    ps = zip s $ tail s
    ph = Map.fromListWith (+) $ zip ps $ repeat 1
    ph' = (!! 40) $ iterate (applyRules m) ph
    pch = Map.unionWith (+) (Map.mapKeysWith (+) fst ph') (Map.mapKeysWith (+) snd ph')
    ch = Map.adjust succ (head s) $ Map.adjust succ (last s) $ Map.map (`div` 2) $ pch
    counts = Map.elems ch
