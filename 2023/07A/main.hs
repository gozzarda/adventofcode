module Main where

import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (group, sort, sortBy)

type Card = Int

type Hand = [Card]

type Prob = [(Hand, Int)]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map readPlay . lines

readPlay :: String -> (Hand, Int)
readPlay s = let (hw : bw : _) = words s in (map charCard hw, read bw)

charCard :: Char -> Card
charCard 'A' = 14
charCard 'K' = 13
charCard 'Q' = 12
charCard 'J' = 11
charCard 'T' = 10
charCard d = digitToInt d

solve :: Prob -> Soln
solve = sum . zipWith (*) [1 ..] . map snd . sortBy (compareHands `on` fst)

data Type = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind deriving (Ord, Eq)

handType :: Hand -> Type
handType h = case sort $ map length $ group $ sort h of
  [5] -> FiveKind
  [1, 4] -> FourKind
  [2, 3] -> FullHouse
  [1, 1, 3] -> ThreeKind
  [1, 2, 2] -> TwoPair
  [1, 1, 1, 2] -> OnePair
  _ -> HighCard

compareHands :: Hand -> Hand -> Ordering
compareHands l r = case compare (handType l) (handType r) of
  LT -> LT
  EQ -> compare l r
  GT -> GT
