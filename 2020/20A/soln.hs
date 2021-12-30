import           Data.List       (group, sort)
import           Data.List.Split (splitOn)
import           Data.Map        (Map, (!))
import qualified Data.Map        as Map

import           Debug.Trace

type Case = [(Int, [[Char]])]
type Soln = Int

main :: IO ()
main = interact $ showSoln . solve . readCase

readCase :: String -> Case
readCase s = zip is gs
  where
    ls = lines s
    ts = splitOn [[]] ls
    is = map (read . init . (drop $ length "Tile ") . head) $ ts
    gs = map tail ts

showSoln :: Soln -> String
showSoln = unlines . return . show

canonize :: (Ord a) => [a] -> [a]
canonize xs = if reverse xs < xs then reverse xs else xs

edges :: [[a]] -> [[a]]
edges xss = [head xss, last xss, map head xss, map last xss]

solve :: Case -> Soln
solve ts = product is'
  where
    is = map fst ts
    ess = map (map canonize . edges . snd) ts
    cs = Map.fromList $ map (\xs -> (head xs, length xs)) $ group $ sort $ concat ess
    css = map (map (cs !)) ess
    is' = map fst $ filter ((==2) . length . filter (==1) . snd) $ zip is css
