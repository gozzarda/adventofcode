module Main where

import Control.Applicative (liftA2)
import Data.Maybe
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

type Prob = [(Int, Int)]

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

parseMul :: ReadP (Int, Int)
parseMul = do
  string "mul("
  lhs <- parse
  char ','
  rhs <- parse
  char ')'
  return (lhs, rhs)

nextMatch :: ReadP a -> ReadP a
nextMatch p = p <++ (get *> nextMatch p)

allMatches :: ReadP a -> ReadP [a]
allMatches p = liftA2 (:) (nextMatch p) (allMatches p) <++ (many get >> return [])

parseProb :: ReadP Prob
parseProb = allMatches parseMul

solve :: Prob -> Soln
solve = sum . map (uncurry (*))
