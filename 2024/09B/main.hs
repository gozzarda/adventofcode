module Main where

import Control.Applicative (liftA2, (<|>))
import Data.Char (digitToInt, isDigit)
import Data.List (partition)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

type Prob = [Int]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = map digitToInt . takeWhile isDigit

solve :: Prob -> Soln
solve ws = sum $ map (\(i, r) -> i * rangeSum r) irs
  where
    is = scanl (+) 0 ws
    rs = zip is $ tail is
    (frs, srs) = deinterleave rs
    ris = zip frs [0 ..]
    irs = go [] (rsFromList (0, sum ws) srs) (reverse ris)
    go irs srs ((r, i) : ris) = case rsQuery (width r) srs of
      Just (l, u) | (l, u) < r -> go (ir : irs) srs' ris
        where
          m = l + width r
          ir = (i, (l, m))
          srs' = (if u > m then rsInsert (m, u) else id) $ rsDelete (l, u) srs
      _ -> go ((i, r) : irs) srs ris
    go irs _ [] = irs
    rangeSum (l, u) = (l + u - 1) * (u - l) `div` 2

deinterleave :: [a] -> ([a], [a])
deinterleave (x : xs) = let (os, es) = deinterleave xs in (x : es, os)
deinterleave [] = ([], [])

-- [lwr, upr)
type Range = (Int, Int)

width :: Range -> Int
width (l, u) = u - l

-- Segment tree of ranges grouped by size, reduction is leftmost
data RangeSet = Nil | Leaf Int (Set Range) | Tree (Int, Int) (Maybe Range) RangeSet RangeSet

leftmost :: Maybe Range -> Maybe Range -> Maybe Range
leftmost x y = liftA2 min x y <|> x <|> y

-- Get the leftmost range in the RangeSet
rsLeftmost :: RangeSet -> Maybe Range
rsLeftmost Nil = Nothing
rsLeftmost (Leaf _ rs) = Set.lookupMin rs
rsLeftmost (Tree _ mr _ _) = mr

-- Construct a RangeSet from some size bounds and a list of Ranges
rsFromList :: (Int, Int) -> [Range] -> RangeSet
rsFromList (l, u) _ | u <= l = Nil
rsFromList (l, u) rs | succ l == u = Leaf l (Set.fromList rs)
rsFromList (l, u) rs = Tree (l, u) mr lt rt
  where
    m = div (l + u) 2
    (lrs, rrs) = partition ((< m) . width) rs
    lt = rsFromList (l, m) lrs
    rt = rsFromList (m, u) rrs
    mr = leftmost (rsLeftmost lt) (rsLeftmost rt)

-- Find the leftmost range of size at least w
rsQuery :: Int -> RangeSet -> Maybe Range
rsQuery _ Nil = Nothing
rsQuery w (Leaf i rs) = if i < w then Nothing else Set.lookupMin rs
rsQuery w (Tree (l, _) mr _ _) | w <= l = mr
rsQuery w (Tree (l, _) _ lt rt) = leftmost (rsQuery w lt) (rsQuery w rt)

-- Delete range from RangeSet
rsDelete :: Range -> RangeSet -> RangeSet
rsDelete _ Nil = Nil
rsDelete r t@(Leaf i _) | width r /= i = t
rsDelete r (Leaf i rs) = Leaf i $ Set.delete r rs
rsDelete r t@(Tree (l, u) _ _ _) | width r < l || u <= width r = t
rsDelete r (Tree lu _ lt rt) = Tree lu mr lt' rt'
  where
    lt' = rsDelete r lt
    rt' = rsDelete r rt
    mr = leftmost (rsLeftmost lt') (rsLeftmost rt')

-- Insert range into RangeSet
rsInsert :: Range -> RangeSet -> RangeSet
rsInsert _ Nil = Nil
rsInsert r t@(Leaf i _) | width r /= i = t
rsInsert r (Leaf i rs) = Leaf i $ Set.insert r rs
rsInsert r t@(Tree (l, u) _ _ _) | width r < l || u <= width r = t
rsInsert r (Tree lu _ lt rt) = Tree lu mr lt' rt'
  where
    lt' = rsInsert r lt
    rt' = rsInsert r rt
    mr = leftmost (rsLeftmost lt') (rsLeftmost rt')
