module Main where

import Data.Array.IArray (Array)
import qualified Data.Array.IArray as Arr
import Data.Bifunctor (bimap)
import Data.List (intercalate, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map

data Symb = Unknown | Operational | Damaged deriving (Ord, Eq, Show)

type Inst = ([Symb], [Int])

type Prob = [Inst]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map readInst . lines

readInst :: String -> Inst
readInst l = (rs, ss)
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
solve = sum . map (dp . bimap (intercalate [Unknown] . replicate 5) (concat . replicate 5))

-- (Pattern, Chunks)
type Context = (Array Int Symb, Array Int Int)

-- (Length of matched prefix of pattern, (Chunk index, Amount of chunk used))
type State = (Int, (Int, Int))

-- Number of valid ways of finishing the pattern
type Value = Int

makeContext :: Inst -> Context
makeContext (mbs, ns) = (mba, na)
  where
    mba = Arr.listArray (0, length mbs - 1) mbs
    na = Arr.listArray (0, length ns - 1) ns

makeStateSpace :: Inst -> Map State ()
makeStateSpace (mbs, ns) = Map.fromList $ zip states $ repeat ()
  where
    chunkStates = [(i, x) | (i, n) <- zip [0 ..] ns, x <- [0 .. n]]
    states = [(l, c) | l <- [0 .. length mbs], c <- chunkStates]

dp :: Inst -> Value
dp inst = (Map.!) dpt (0, (0, 0))
  where
    context = makeContext inst
    statespace = makeStateSpace inst
    dpt = Map.mapWithKey (const . dpf dpt context) statespace

-- In desperate need of a refactor
dpf :: Map State Value -> Context -> State -> Value
dpf dpt (pattern, chunks) (preflen, (chunki, chunkn))
  | chunksDone && patternDone = 1 -- Base case
  | chunksDone && nextSymbol /= Damaged = takeOperational -- May append Operational until done
  | chunksDone || patternDone = 0 -- One finished before other
  | chunkDone && nextSymbol == Damaged = 0 -- Finished chunk before pattern
  | chunkDone = takeOperationalNextChunk -- Start Operational gap before next Damaged chunk
  | chunkStarted && nextSymbol == Operational = 0 -- Can't end a chunk early
  | nextSymbol == Operational = takeOperational -- May extend Operational gap before Damaged chunk
  | chunkStarted || nextSymbol == Damaged = takeDamaged -- Must extend Damaged chunk
  | otherwise = takeOperational + takeDamaged -- May either extend Operational gap or start Damaged chunk
  where
    nextSymbol = (Arr.!) pattern preflen
    patternDone = preflen == 1 + (uncurry subtract $ Arr.bounds pattern)
    chunkDone = chunkn == (Arr.!) chunks chunki
    chunksDone = chunki == (snd $ Arr.bounds chunks) && chunkDone
    chunkStarted = chunkn > 0
    takeOperational = (Map.!) dpt (preflen + 1, (chunki, chunkn))
    takeOperationalNextChunk = (Map.!) dpt (preflen + 1, (chunki + 1, 0))
    takeDamaged = (Map.!) dpt (preflen + 1, (chunki, chunkn + 1))
