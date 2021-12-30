import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Char          (isDigit)
import           Data.List          (sort)
-- import Data.IntMap (IntMap, (!))
-- import qualified Data.IntMap as IntMap

import           Debug.Trace

type Case = [Int]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map (read . return) . filter isDigit

showSoln :: Soln -> String
showSoln = unlines . return . show

numCups :: Int
numCups = 1000000

numSteps :: Int
numSteps = 10000000

-- type State = (Int, IntMap IntMap.Key)

-- initState :: Case -> State
-- initState cs = (head cs, IntMap.fromList $ zip cs' $ tail $ cycle cs')
--   where
--     cs' = cs ++ (drop (length cs) [1..numCups])

-- step :: State -> State
-- step (curr, lm) = (next, lm'')
--   where
--     [cupa, cupb, cupc, next] = take 4 $ tail $ iterate (lm !) curr
--     lm' = IntMap.insert curr next lm
--     dest = head $ dropWhile (`elem` [cupa, cupb, cupc]) $ map (succ . (flip mod numCups) . pred) $ tail $ iterate pred curr
--     dest' = lm' ! dest
--     lm'' = IntMap.insert dest cupa $ IntMap.insert cupc dest' lm'

result :: Case -> UArray Int Int
result cs = runSTUArray $ do
  links <- newArray (0, numCups) 0
  let cs' = cs ++ (drop (length cs) [1..numCups])
  let kvs = zip cs' $ tail $ cycle cs'
  writeArray links 0 (head cs)
  forM_ kvs $ \(k, v) -> writeArray links k v
  forM_ [1..numSteps] $ \_ -> do
    curr <- readArray links 0
    cupa <- readArray links curr
    cupb <- readArray links cupa
    cupc <- readArray links cupb
    next <- readArray links cupc
    writeArray links curr next
    let dest = head $ dropWhile (`elem` [cupa, cupb, cupc]) $ map (succ . (flip mod numCups) . pred) $ tail $ iterate pred curr
    dest' <- readArray links dest
    writeArray links dest cupa
    writeArray links cupc dest'
    writeArray links 0 next
  return links

solve :: Case -> Soln
solve cups = traceShow (x, y) $ x * y
  where
    arr = result cups
    x = arr ! 1
    y = arr ! x
