module Main where

import Data.Array.IArray (Array)
import qualified Data.Array.IArray as Arr
import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe)

data Glyph = Unknown | Operational | Damaged deriving (Ord, Eq, Show)

type Record = ([Glyph], [Int])

type Prob = [Record]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map readRecord . lines

readRecord :: String -> Record
readRecord l = (rs, ss)
  where
    (rw, sw) = splitMatch " " l
    rs = map charFlag rw
    charFlag '?' = Unknown
    charFlag '.' = Operational
    charFlag '#' = Damaged
    ss = map read $ splitMatches "," sw

splitMatch :: (Eq a) => [a] -> [a] -> ([a], [a])
splitMatch d = go []
  where
    go pref [] = (reverse pref, [])
    go pref t@(x : xs) = case stripPrefix d t of
      Just suff -> (reverse pref, suff)
      Nothing -> go (x : pref) xs

splitMatches :: (Eq a) => [a] -> [a] -> [[a]]
splitMatches d xs = case splitMatch d xs of
  (pref, []) -> [pref]
  (pref, suff) -> pref : splitMatches d suff

solve :: Prob -> Soln
solve = sum . map dp

-- (Pattern, Max chunk len from index, Chunk sizes)
type Context = (Array Int Glyph, Array Int Int, Array Int Int)

-- (Length of matched prefix of pattern, Number of chunks placed)
type State = (Int, Int)

-- Number of valid ways of finishing the pattern
type Value = Int

makeContext :: Record -> Context
makeContext (glyphs, chunks) = (glypha, limita, chunka)
  where
    glypha = Arr.listArray (0, length glyphs - 1) glyphs
    limits = foldr (\s ns@(n : _) -> (if s == Operational then 0 else n + 1) : ns) [0] glyphs
    limita = Arr.listArray (0, length limits - 1) limits
    chunka = Arr.listArray (0, length chunks - 1) chunks

makeStateSpace :: Record -> Map State ()
makeStateSpace (glyphs, chunks) = Map.fromList $ zip states $ repeat ()
  where
    states = [(l, c) | l <- [0 .. length glyphs], c <- [0 .. length chunks]]

dp :: Record -> Value
dp (glyphs, chunks) = (Map.!) dpt (0, 0)
  where
    record = (glyphs ++ [Operational], chunks)
    context = makeContext record
    statespace = makeStateSpace record
    dpt = Map.mapWithKey (const . dpf dpt context) statespace

dpf :: Map State Value -> Context -> State -> Value
dpf dpt (glyphs, limits, chunks) (glyphi, chunki)
  | chunk == Nothing && glyph == Nothing = 1
  | otherwise = sum $ catMaybes [takeOperational, takeDamagedChunk]
  where
    glyph = (Arr.!?) glyphs glyphi
    limit = (Arr.!) limits glyphi
    chunk = (Arr.!?) chunks chunki
    takeOperational = if maybe True (== Damaged) glyph then Nothing else Just $ (Map.!) dpt (glyphi + 1, chunki)
    takeDamagedChunk =
      if maybe False (/= Operational) glyph && maybe False (<= limit) chunk && (Arr.!) glyphs (glyphi + fromJust chunk) /= Damaged
        then Just $ (Map.!) dpt (glyphi + fromJust chunk + 1, chunki + 1)
        else Nothing
