import           Control.Monad.State.Lazy
import           Data.Char                (isDigit, isSpace)
import           Data.List                (uncons)
import           Data.Maybe               (fromJust)

type Scanner = State String

scanchar :: Scanner Char
scanchar = do
  (v, s) <- fromJust <$> uncons <$> get
  put s
  return v

scanwhile :: (Char -> Bool) -> Scanner String
scanwhile cond = do
  (v, s) <- span cond <$> get
  put s
  return v

scanint :: Scanner Int
scanint = read <$> scanwhile isDigit

scanspace :: Scanner String
scanspace = scanwhile isSpace

scanword :: Scanner String
scanword = scanspace >> scanwhile (not.isSpace)

type Case = [(Int, Int, Char, String)]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = map parseline . lines

parseline :: String -> (Int, Int, Char, String)
parseline = evalState $ do
  l <- scanint
  scanchar
  u <- scanint
  scanspace
  c <- scanchar
  scanchar
  s <- scanword
  return (l, u, c, s)

display :: Soln -> String
display = unlines . singleton . show

singleton :: a -> [a]
singleton x = [x]

solve :: Case -> Soln
solve = length . filter id . map solve'

solve' :: (Int, Int, Char, String) -> Bool
solve' (l, u, c, s) = lv /= uv
  where
    lc = s !! (l - 1)
    uc = s !! (u - 1)
    lv = lc == c
    uv = uc == c
