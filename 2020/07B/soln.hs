import           Data.List  (findIndices)
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe, isJust)
import           Text.Read  (readMaybe)

type Case = Map String [(Int, String)]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = Map.fromList . (map parseRule) . lines

parseRule :: String -> (String, [(Int, String)])
parseRule s = (p, bs)
  where
    ws = words s
    p = unwords $ take 2 ws
    is = findIndices isInt ws
    bs = map (\i -> f $ take 3 $ drop i ws) is
    f bws = (read $ head bws, unwords $ tail bws)

isInt :: String -> Bool
isInt = isJust . (readMaybe :: String -> Maybe Int)

display :: Soln -> String
display = unlines . return . show

solve :: Case -> Soln
solve = (Map.findWithDefault (-1) "shiny gold") . numContained

numContained :: Case -> Map String Int
numContained rules = totals
  where
    totals = Map.map total rules
    total cs = sum $ map (\(n, s) -> n + n * (Map.findWithDefault 0 s totals)) cs
