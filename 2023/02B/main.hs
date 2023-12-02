module Main where

import Data.Char (isAlpha, isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Tuple (swap)
import Debug.Trace
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

type Hand = [(Int, String)]

type Game = (Int, [Hand])

type Prob = [Game]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb =
  fst
    . fromMaybe (error "Failed to parse problem")
    . listToMaybe
    . ReadP.readP_to_S (parseProb <* ReadP.skipSpaces <* ReadP.eof)

parseNat :: ReadP Int
parseNat = read <$> (ReadP.many1 $ ReadP.satisfy isDigit)

parseWord :: ReadP String
parseWord = ReadP.munch1 isAlpha

parseCubes :: ReadP (Int, String)
parseCubes = do
  count <- parseNat
  ReadP.skipSpaces
  colour <- parseWord
  return (count, colour)

parseHand :: ReadP Hand
parseHand = ReadP.sepBy1 parseCubes (ReadP.char ',' <* ReadP.skipSpaces)

parseGame :: ReadP Game
parseGame = do
  ReadP.string "Game"
  ReadP.skipSpaces
  game_id <- parseNat
  ReadP.char ':'
  ReadP.skipSpaces
  hands <- ReadP.sepBy1 parseHand (ReadP.char ';' <* ReadP.skipSpaces)
  return (game_id, hands)

parseProb :: ReadP Prob
parseProb = ReadP.sepBy1 parseGame (ReadP.char '\n')

solve :: Prob -> Soln
solve = sum . map histPower . map gameToHist

handToHist :: Hand -> Map String Int
handToHist = Map.fromListWith (+) . map swap

histsMax :: [Map String Int] -> Map String Int
histsMax = Map.unionsWith max

gameToHist :: Game -> Map String Int
gameToHist = histsMax . map handToHist . snd

histPower :: Map String Int -> Int
histPower = Map.foldr' (*) 1
