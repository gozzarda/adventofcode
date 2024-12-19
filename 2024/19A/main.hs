module Main where

import Data.List (stripPrefix, tails)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

type Prob = ([String], [String])

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

parseProb :: ReadP Prob
parseProb = do
  let word = munch1 (`elem` "wubrg")
  patterns <- sepBy word (string ", ")
  skipSpaces
  designs <- sepBy word skipSpaces
  return (patterns, designs)

solve :: Prob -> Soln
solve (patterns, designs) = Map.size $ Map.filter id solns
  where
    dps = concatMap tails designs
    dpt = Map.map dpf $ Map.fromList $ zip dps dps
    dpf "" = True
    dpf s = any (dpt Map.!) $ mapMaybe (`stripPrefix` s) patterns
    solns = Map.restrictKeys dpt (Set.fromList designs)
