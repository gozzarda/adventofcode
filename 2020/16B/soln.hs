import           Data.Char       (isSpace)
import           Data.List       (dropWhileEnd, isPrefixOf, sort, transpose)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as Map

import           Debug.Trace

data Case = Case { rules :: Map String [(Int, Int)], ticket :: [Int], tickets :: [[Int]] }
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse s = Case { rules = parseRules rp, ticket = parseTicket (last tp), tickets = map parseTicket (tail tsp) }
  where
    (rp:tp:tsp:_) = paras s

paras :: String -> [[String]]
paras = splitOn [[]] . lines

parseRules :: [String] -> Map String [(Int, Int)]
parseRules = Map.fromList . map parseRule

parseRule :: String -> (String , [(Int, Int)])
parseRule s = (ks, bs)
  where
    (ks:vs:_) = splitOn ":" s
    bss = map trim $ splitOn "or" vs
    bs = map parseBound bss

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd (isSpace)

parseBound :: String -> (Int, Int)
parseBound s = (l, u)
  where
    (l:u:_) = map read $ splitOn "-" s

parseTicket :: String -> [Int]
parseTicket = map read . splitOn ","

display :: Soln -> String
display = unlines . return . show

valids :: Case -> [[Int]]
valids c = filter (all bsf) (tickets c)
  where
    bs = concat $ Map.elems (rules c)
    bf (l, u) v = l <= v && v <= u
    bsf v = any (flip bf v) bs

rulefs :: Case -> Map String (Int -> Bool)
rulefs c = Map.map bsf (rules c)
  where
    bf v (l, u) = l <= v && v <= u
    bsf bs v = any (bf v) bs

permsat :: (Eq a) => [[a]] -> [[a]]
permsat = permsat' []
  where
    permsat' _ [] = [[]]
    permsat' s (o:os) = concat $ map f o'
      where
        o' = filter (not . (`elem` s)) o
        f v = map (v:) $ permsat' (v:s) os

solve :: Case -> Soln
solve c = product $ map snd $ filter (isPrefixOf "departure" . fst) $ zip p' (ticket c)
  where
    ts = valids c
    cs = transpose ts
    rs = Map.assocs $ rulefs c
    osf vs = map fst $ filter (\(k, r) -> all r vs) rs
    os = map osf cs
    ls = map length os
    slos = map snd $ sort $ zip ls os
    slis = map snd $ sort $ zip ls [0..]
    ps = permsat slos
    p = if length ps == 1 then head ps else error "Multiple solutions"
    p' = traceShowId $ map snd $ sort $ zip slis p
