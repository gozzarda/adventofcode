module Main where

import Data.Char (isLower, ord)
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

data Oper = Set String Int | Del String

type Prob = [Oper]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map readOper . splitMatches "," . concat . lines

readOper :: String -> Oper
readOper str = let (key, opstr) = span isLower str in case opstr of
  "-" -> Del key
  ('=':valstr) -> Set key (read valstr)

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
solve ops = sum [(bi + 1) * (si + 1) * v | (bi, kvs) <- hashmapToList hm, (si, (_, v)) <- zip [0..] kvs]
  where
    opf (Set k v) = hashmapAlter (const $ Just v) k
    opf (Del k) = hashmapAlter (const Nothing) k
    hm = foldl (&) hashmapEmpty (map opf ops)

type Bucket a = [(String, a)]

type HashMap a = IntMap (Bucket a)

hash :: String -> Word8
hash = foldl (\h c -> let x = fromIntegral $ ord c in (h + x) * 17) 0

hashInt :: String -> Int
hashInt = fromIntegral . hash

bucketAlter :: (Maybe a -> Maybe a) -> String -> Bucket a -> Bucket a
bucketAlter f key [] = case f Nothing of
  Just v -> [(key, v)]
  Nothing -> []
bucketAlter f key (kv@(k, v):kvs)
  | k /= key = kv : bucketAlter f key kvs
  | otherwise = case f (Just v) of
    Just v' -> (k, v') : kvs
    Nothing -> kvs

hashmapEmpty :: HashMap a
hashmapEmpty = IntMap.empty

hashmapAlter :: (Maybe a -> Maybe a) -> String -> HashMap a -> HashMap a
hashmapAlter f key = let i = hashInt key in IntMap.alter (nonempty . bucketAlter f key . fromMaybe []) i

nonempty :: [a] -> Maybe [a]
nonempty [] = Nothing
nonempty xs = Just xs

hashmapToList :: HashMap a -> [(Int, Bucket a)]
hashmapToList = IntMap.assocs
