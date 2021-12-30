import           Data.List   (intercalate)
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Set    (Set)
import qualified Data.Set    as Set

import           Debug.Trace

type Case = [([String], [String])]
type Soln = String

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readItem . lines

readItem :: String -> ([String], [String])
readItem s = (is, tail as)
  where
    s' = filter (not . (`elem` "(,)")) s
    ws = words s'
    (is, as) = break (=="contains") ws

showSoln :: Soln -> String
showSoln = unlines . return

candidates :: Case -> Map String (Set String)
candidates ps = Map.fromListWith (Set.intersection) aiss
  where
    (iss, ass) = unzip ps
    aiss = concat $ zipWith (map . (flip (,)) . Set.fromList) iss ass

solution :: Map String (Set String) -> Map String String
solution aism = if Map.null aism then Map.empty else Map.union uniques $ solution remainder
  where
    (uniques'', remainder') = Map.partition ((==1) . Set.size) aism
    uniques' = Map.map Set.findMin uniques''
    uniques = if Map.null uniques' then error "Failed to find uniques" else uniques'
    removed = Set.fromList $ Map.elems uniques
    remainder = Map.map (flip Set.difference removed) remainder'

solve :: Case -> Soln
solve ps = traceShow aim $ dis
  where
    aim = solution $ candidates ps
    dis = intercalate "," $ Map.elems aim
