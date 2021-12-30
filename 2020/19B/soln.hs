import           Data.IntMap     (IntMap, (!))
import qualified Data.IntMap     as IntMap
import           Data.List       (uncons)
import           Data.List.Split (splitOn)

import           Debug.Trace

data AndOr a = Leaf a | And [AndOr a] | Or [AndOr a] deriving (Show)
type Spec = Either Char (AndOr Int)
type Case = (IntMap Spec, [String])
type Soln = Int

main :: IO ()
main = interact $ showSoln . solve . readCase

readCase :: String -> Case
readCase s = (pm, tail ss)
  where
    (pss, ss) = break null $ lines s
    pm = IntMap.fromList $ map readSpec pss

maybeToLeft :: b -> Maybe a -> Either a b
maybeToLeft _ (Just l) = Left l
maybeToLeft r Nothing  = Right r

readSpec :: String -> (Int, Spec)
readSpec s = (id, maybeToLeft ps mc)
  where
    (ids,_:rs) = break (==':') s
    id = read ids
    pss = splitOn "|" rs
    iss = map (map read . words) pss
    ps = Or $ map (And . map Leaf) iss
    mc = fmap (head . snd) $ uncons $ dropWhile (/='"') rs

showSoln :: Soln -> String
showSoln = unlines . return . show

type Rule = AndOr Char

makeRule :: IntMap Spec -> Rule
makeRule specs = m ! 0
  where
    m = fmap f specs
    f (Left c)   = Leaf c
    f (Right ao) = g ao
    g (Leaf i)  = m ! i
    g (And aos) = And $ map g aos
    g (Or aos)  = Or $ map g aos

matchRule :: Rule -> String -> [String]
matchRule (Leaf rc) []     = []
matchRule (Leaf rc) (c:cs) = if c == rc then [cs] else []
matchRule (And aos) s      = foldl (flip concatMap) [s] (map matchRule aos)
matchRule (Or aos) s       = concatMap (flip matchRule s) aos

listToAndOr :: [[a]] -> AndOr a
listToAndOr = Or . map (And . map Leaf)

changes :: IntMap Spec
changes = IntMap.fromList [
  (8, Right $ listToAndOr [[42], [42, 8]]),
  (11, Right $ listToAndOr [[42, 31], [42, 11, 31]])
  ]

solve :: Case -> Soln
solve (pm, ss) = length $ filter f ss
  where
    pm' = IntMap.union changes pm
    r = makeRule pm'
    f = elem [] . matchRule r
