{-# LANGUAGE RankNTypes #-}

module Main where

import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

data Packet = Atom Int | List [Packet]

type Case = [(Packet, Packet)]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = pairs . map read . words

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (l : r : xs) = (l, r) : pairs xs
pairs _ = error "pairs: Odd length list"

parsePacket :: ReadP Packet
parsePacket = parseAtom +++ parseList
  where
    parseAtom = Atom <$> ReadPrec.readPrec_to_P readPrec ReadPrec.minPrec
    parseList = List <$> between (char '[') (char ']') (sepBy parsePacket (char ','))

instance Read Packet where
  readPrec = ReadPrec.lift parsePacket

showSoln :: Soln -> String
showSoln = unlines . return . show

wrap :: Packet -> Packet
wrap p@(Atom _) = List [p]
wrap p = p

instance Eq Packet where
  (==) (Atom lv) (Atom rv) = lv == rv
  (==) (List ls) (List rs) = ls == rs
  (==) l r = wrap l == wrap r

instance Ord Packet where
  compare (Atom lv) (Atom rv) = compare lv rv
  compare (List ls) (List rs) = compare ls rs
  compare l r = compare (wrap l) (wrap r)

solve :: Case -> Soln
solve = sum . map fst . filter ((/= GT) . snd) . zip [1 ..] . map (uncurry compare)
