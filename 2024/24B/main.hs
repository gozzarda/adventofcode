module Main where

import Data.Bits (shift, xor)
import Data.Char (isDigit, isSpace)
import Data.Functor (($>))
import Data.List (stripPrefix)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Debug.Trace
import Numeric (showBin)
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

data Expr
  = Atom Bool
  | And String String
  | Or String String
  | Xor String String
  deriving (Ord, Eq, Show)

type Prob = [(String, Expr)]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = doReadP' $ parseProb <* eof

doReadP' :: ReadP a -> String -> a
doReadP' p = fromMaybe (error "Parsing failed") . doReadP p

doReadP :: ReadP a -> String -> Maybe a
doReadP p = fmap fst . listToMaybe . readP_to_S p

parse :: (Read a) => ReadP a
parse = ReadPrec.readPrec_to_P readPrec ReadPrec.minPrec

parseBool :: ReadP Bool
parseBool = (char '0' $> False) +++ (char '1' $> True)

parseAtom :: ReadP (String, Expr)
parseAtom = do
  name <- munch (/= ':')
  char ':'
  skipSpaces
  val <- parseBool
  return (name, Atom val)

parseOp :: ReadP (String -> String -> Expr)
parseOp =
  choice
    [ string "AND" $> And,
      string "OR" $> Or,
      string "XOR" $> Xor
    ]

parseGate :: ReadP (String, Expr)
parseGate = do
  lname <- munch (not . isSpace)
  skipSpaces
  op <- parseOp
  skipSpaces
  rname <- munch (not . isSpace)
  skipSpaces
  string "->"
  skipSpaces
  name <- munch (not . isSpace)
  return (name, op lname rname)

parseExpr :: ReadP (String, Expr)
parseExpr = parseAtom +++ parseGate

parseProb :: ReadP Prob
parseProb = endBy parseExpr skipSpaces

solve :: Prob -> Soln
solve kxs = traceShow (showBin d "") z
  where
    kxm = Map.fromList kxs
    kvm = Map.map eval kxm
    eval expr = case expr of
      Atom b -> b
      And lhs rhs -> (kvm Map.! lhs) && (kvm Map.! rhs)
      Or lhs rhs -> (kvm Map.! lhs) || (kvm Map.! rhs)
      Xor lhs rhs -> (kvm Map.! lhs) /= (kvm Map.! rhs)
    kvs = Map.assocs kvm
    bits pref = mapMaybe (\(k, v) -> (,v) . read <$> stripPrefix pref k) kvs
    num pref = sum $ map (shift 1 . fst) $ filter snd $ bits pref
    x = num "x"
    y = num "y"
    z = num "z"
    d = (x + y) `xor` z
