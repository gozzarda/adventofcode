module Main where

import Control.Applicative (liftA2)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

type Prob = ([(Int, Int)], [[Int]])

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

parseRule :: ReadP (Int, Int)
parseRule = (,) <$> parse <* char '|' <*> parse

parsePages :: ReadP [Int]
parsePages = sepBy parse $ char ','

parseProb :: ReadP Prob
parseProb = do
  rules <- sepBy parseRule $ char '\n'
  skipSpaces
  pagess <- sepBy parsePages $ char '\n'
  return (rules, pagess)

solve :: Prob -> Soln
solve (rules, pagess) = sum $ map mid $ filter (isOrdered rules) pagess
  where
    mid xs = xs !! (length xs `div` 2)

isOrdered :: [(Int, Int)] -> [Int] -> Bool
isOrdered rules pages = all obeyed rules
  where
    pim = Map.fromList $ zip pages [0 ..]
    obeyed (l, u) = fromMaybe True $ liftA2 (<) li ui
      where
        li = Map.lookup l pim
        ui = Map.lookup u pim
