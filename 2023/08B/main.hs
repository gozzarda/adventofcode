module Main where

import Data.Char (isAlphaNum)
import Data.List (findIndices, foldl1, intersect)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Vert = String

data Turn = L | R deriving (Ord, Eq, Show, Read)

type Prob = ([Turn], [(Vert, (Vert, Vert))])

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb s = (readTurns tl, map readVert vls)
  where
    (tl : _ : vls) = lines s

readTurns :: String -> [Turn]
readTurns = map (read . (: []))

readVert :: String -> (Vert, (Vert, Vert))
readVert s = let (v : l : r : _) = filter (not . null) $ map (filter isAlphaNum) $ words s in (v, (l, r))

-- The given input actually only requires LCM of cycle length, but let's do it properly
solve :: Prob -> Soln
solve (ts, vlrs) = head $ (++) preis $ map (+ (length $ head pres)) $ fst cycis
  where
    vlrm = Map.fromList vlrs
    srcs = filter ((== 'A') . last) $ map fst vlrs
    (pres, cycs) = unzip $ alignCycles $ map (findCycle vlrm ts) srcs
    preis = foldl1 intersect $ map (findIndices ((== 'Z') . last)) pres
    cycis = foldl1 matchCycles $ map (cycleIndices ((== 'Z') . last)) cycs

findCycle :: Map Vert (Vert, Vert) -> [Turn] -> Vert -> ([Vert], [Vert])
findCycle vlrm ts src = (take cl path, drop cl $ take cu path)
  where
    step (v, (t : ts)) = (select t $ (Map.!) vlrm v, ts)
    path = fst $ unzip $ iterate step (src, cycle ts)
    states = zip path $ cycle [1 .. length ts]
    (cl : cu : _) = map (+ 1) $ findIndices id $ zipWith (==) states $ evens states

select :: Turn -> (a, a) -> a
select L (l, _) = l
select R (_, r) = r

evens :: [a] -> [a]
evens (x : _ : xs) = x : evens xs

alignCycles :: [([a], [a])] -> [([a], [a])]
alignCycles cycs = map align cycs
  where
    plen = maximum $ map (length . fst) cycs
    align (p, c) = let pc = p ++ cycle c in (take plen $ pc, take (length c) $ drop plen pc)

cycleIndices :: (a -> Bool) -> [a] -> ([Int], Int)
cycleIndices f c = (findIndices f c, length c)

matchCycles :: ([Int], Int) -> ([Int], Int) -> ([Int], Int)
matchCycles ([], _) _ = ([], 0)
matchCycles _ ([], _) = ([], 0)
matchCycles (lis, ll) (ris, rl) = (mis, ml)
  where
    ml = lcm ll rl
    nextFrom (is, l) i = head $ dropWhile (< i) $ concat $ zipWith (map . (+)) (map (* l) [(div i l) ..]) $ repeat is
    mis = go 0
    go i = if max nli nri >= ml then [] else if nli == nri then nli : go (nni + 1) else go nni
      where
        nli = nextFrom (lis, ll) i
        nri = nextFrom (ris, rl) i
        nni = max nli nri
