import           Control.Applicative (liftA2)
import           Data.Either         (rights)
import           Data.IntMap         (IntMap, (!))
import qualified Data.IntMap         as IntMap
import           Data.List           (uncons)
import           Data.List.Split     (splitOn)
import           Data.Maybe          (catMaybes)
import           Text.Parsec
import           Text.Parsec.String

import           Debug.Trace

type Case = (IntMap (Either Char [[Int]]), [String])
type Soln = Int

main :: IO ()
main = interact $ showSoln . solve . readCase

readCase :: String -> Case
readCase s = (pm, tail ss)
  where
    (pss, ss) = break (null) $ lines s
    pm = IntMap.fromList $ map readSpec pss

maybeToLeft :: b -> Maybe a -> Either a b
maybeToLeft _ (Just l) = Left l
maybeToLeft r Nothing  = Right r

readSpec :: String -> (Int, Either Char [[Int]])
readSpec s = (i, maybeToLeft ps mc)
  where
    (is,_:rs) = break (==':') s
    i = read is
    pss = splitOn "|" rs
    ps = map (map read . words) pss
    mc = fmap (head . snd) $ Data.List.uncons $ dropWhile (/='"') rs

showSoln :: Soln -> String
showSoln = unlines . return . show

makeRule :: IntMap (Either Char [[Int]]) -> Parser String
makeRule pm = (rm ! 0) <* eof
  where
    rm = IntMap.map f pm
    f (Left c)    = return <$> (char c)
    f (Right iss) = g iss
    g iss = choice $ map (try . h) iss
    h is = foldl1 (liftA2 (++)) $ map (rm !) is

solve :: Case -> Soln
solve (pm, ss) = trace (unlines $ map show ss) $ trace (unlines $ map (('\n':).show) rs) $ length $ rights rs
  where
    r = makeRule pm
    rs = map (parse r "<rule>") ss
