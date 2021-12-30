import           Data.Char       (isSpace)
import           Data.List       (dropWhileEnd)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as Map

-- import Debug.Trace

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

solve :: Case -> Soln
solve c = sum $ filter (not . bsf) vs
  where
    vs = concat (tickets c)
    bs = concat $ Map.elems (rules c)
    bf (l, u) v = l <= v && v <= u
    bsf v = any (flip bf v) bs
