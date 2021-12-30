import           Data.List.Split (splitOn)
import qualified Data.Set        as Set

type Case = [[String]]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = (splitOn [[]]) . lines

display :: Soln -> String
display = unlines . return . show

solve :: Case -> Soln
solve = sum . (map $ Set.size . Set.unions . (map Set.fromList))
