module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Prob = [[Char]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = lines

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (lf, ls) (rf, rs) = (f lf rf, f ls rs)

solve :: Prob -> Soln
solve xss = Set.size $ Set.unions $ map ansf rcss
  where
    nr = length xss
    nc = maximum $ map length xss
    inBounds (r, c) = 0 <= r && r < nr && 0 <= c && c < nc
    xrcs = [(x, (r, c)) | (r, xs) <- zip [0 ..] xss, (c, x) <- zip [0 ..] xs]
    xrcm = Map.fromListWith Set.union $ map (fmap Set.singleton) xrcs
    ercs = Map.findWithDefault Set.empty '.' xrcm
    rcss = Map.elems $ Map.delete '.' xrcm
    ansf :: Set (Int, Int) -> Set (Int, Int)
    ansf rcs = Set.fromList $ concat $ pairsWith f $ Set.toList rcs
      where
        f rc rc' = takeWhile inBounds $ iterate (both2 (+) drc) rc'
          where
            drc' = both2 subtract rc rc'
            drc = let d = max 1 $ uncurry gcd drc' in both (`div` d) drc'

pairsWith :: (a -> a -> b) -> [a] -> [b]
pairsWith f (x : xs) = map (f x) xs ++ map (`f` x) xs ++ pairsWith f xs
pairsWith _ _ = []
