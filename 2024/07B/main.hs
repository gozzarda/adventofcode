module Main where

import Data.Maybe
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

type Prob = [(Int, [Int])]

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

parseTest :: ReadP (Int, [Int])
parseTest = do
  t <- parse
  char ':'
  skipSpaces
  xs <- sepBy parse skipSpaces
  return (t, xs)

parseProb :: ReadP Prob
parseProb = sepBy parseTest (char '\n')

solve :: Prob -> Soln
solve = sum . map fst . filter possible
  where
    possible (t, x : xs) = go x xs
      where
        go acc _ | acc > t = False
        go acc (x : xs) = go (acc * x) xs || go (acc + x) xs || go (decConcat acc x) xs
        go acc _ = acc == t

decConcat :: Int -> Int -> Int
decConcat lhs rhs = lhs * 10 ^ pow + rhs
  where
    pow = length $ takeWhile (/= 0) $ iterate (`div` 10) rhs
