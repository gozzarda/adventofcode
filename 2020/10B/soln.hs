import           Data.List (sort, tails)
import           Data.Map  (Map)
import qualified Data.Map  as Map

type Case = [Integer]
type Soln = Integer

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = (map read) . lines

display :: Soln -> String
display = unlines . return . show

solve :: Case -> Soln
solve vals = (Map.!) opts levels
  where
    levels = reverse $ sort (0:vals)
    opts = Map.fromList $ (\vs -> zip vs (map f vs)) $ tails levels
    f [0] = 1
    f (v:vs) = sum $ map ((Map.!) opts) $ filter ((>=(v-3)) . head) $ filter (not . null) (tails vs)
