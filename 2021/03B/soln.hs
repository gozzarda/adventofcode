type Case = [[Bool]]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map (map (== '1')) . lines

showSoln :: Soln -> String
showSoln = unlines . return . show

-- Leaf count | Branch count weight lsub rsub
data BT = Leaf Int | Branch Int Int BT BT deriving (Show, Eq)

btWeight :: BT -> Int
btWeight (Leaf c)         = c
btWeight (Branch _ w _ _) = w

btEmpty :: BT
btEmpty = Leaf 0

btInsert :: [Bool] -> BT -> BT
btInsert [] (Leaf c) = Leaf (succ c)
btInsert [] (Branch c w l r) = Branch (succ c) (succ w) l r
btInsert (b:bs) (Leaf c) = if b
  then Branch c (succ c) btEmpty (btInsert bs btEmpty)
  else Branch c (succ c) (btInsert bs btEmpty) btEmpty
btInsert (b:bs) (Branch c w l r) = if b
  then Branch c (succ w) l (btInsert bs r)
  else Branch c (succ w) (btInsert bs l) r

btFromList :: [[Bool]] -> BT
btFromList = foldl (flip btInsert) btEmpty

btSelectPath :: (BT -> BT -> Bool) -> BT -> [Bool]
btSelectPath _ (Leaf _) = []
btSelectPath f (Branch _ _ l r) = let dir = f l r in dir : (if dir then btSelectPath f r else btSelectPath f l)

generSelect :: BT -> BT -> Bool
generSelect l r = (btWeight l <= btWeight r)

scrubSelect :: BT -> BT -> Bool
scrubSelect l r | btWeight l == 0 = True
scrubSelect l r | btWeight r == 0 = False
scrubSelect l r = (btWeight l > btWeight r)

boolsToInt :: [Bool] -> Int
boolsToInt = sum . zipWith (*) (iterate (2 *) 1) . map fromEnum . reverse

solve :: Case -> Soln
solve bss = g * s
  where
    t = btFromList bss
    gener = btSelectPath generSelect t
    scrub = btSelectPath scrubSelect t
    g = boolsToInt gener
    s = boolsToInt scrub
