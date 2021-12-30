import           Control.Monad.State.Lazy
import           Data.IntMap              (IntMap)
import qualified Data.IntMap              as IntMap
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IntSet
import           Data.Maybe               (fromJust)
import           Data.Tuple               (swap)

import           Debug.Trace              (trace, traceShow, traceShowId)

data Inst = Acc Int | Jmp Int | Nop Int | Halt deriving (Eq)
type Case = [Inst]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = (map parseInst) . lines

parseInst :: String -> Inst
parseInst s = case os of
    "acc" -> Acc a
    "jmp" -> Jmp a
    "nop" -> Nop a
  where
    (os:as:_) = words s
    a = read $ dropWhile (=='+') as

display :: Soln -> String
display = unlines . return . show

data Sim = Sim { simCode :: IntMap Inst
               , simSeen :: IntSet
               , simPC   :: Int
               , simAcc  :: Int
               }

instance Show Sim where
  show s = show (simPC s, simAcc s)

type SimM = State Sim

instMap :: Case -> IntMap Inst
instMap is = IntMap.fromAscList $ zip [0..] (is ++ [Halt])

initSim :: IntMap Inst -> Sim
initSim is = Sim {simCode = is, simSeen = IntSet.empty, simPC = 0, simAcc = 0}

simInst :: Sim -> Inst
simInst s = fromJust $ IntMap.lookup (simPC s) (simCode s)

succInst :: Int -> Inst -> Int
succInst addr inst = case inst of
  Halt  -> addr
  Jmp a -> addr + a
  _     -> succ addr

stepSim :: Sim -> Sim
stepSim s = traceShowId s'
  where
    s' = s {simSeen = seen, simPC = pc, simAcc = acc}
    seen = IntSet.insert (simPC s) (simSeen s)
    inst = simInst s
    pc = succInst (simPC s) inst
    acc = case inst of
      Acc a -> (simAcc s) + a
      _     -> (simAcc s)

stepSimM :: SimM ()
stepSimM = modify stepSim

runHalting :: SimM Bool
runHalting = do
  inst <- gets simInst
  if inst == Halt
    then return True
    else do
      pc <- gets simPC
      seen <- gets simSeen
      if IntSet.member pc seen
        then return False
        else stepSimM >> runHalting

type Graph = IntMap IntSet

edges :: Graph -> [(Int, Int)]
edges = concat . IntMap.elems . IntMap.mapWithKey (\k v -> zip (repeat k) (IntSet.elems v))

fromEdges :: [(Int, Int)] -> Graph
fromEdges = (IntMap.fromListWith IntSet.union) . (map (\(k, v) -> (k, IntSet.singleton v)))

transpose :: Graph -> Graph
transpose = fromEdges . (map swap) . edges

reachability :: Graph -> [Int] -> IntSet
reachability _ [] = IntSet.empty
reachability g (v:s) = IntSet.insert v $ reachability unvisited $ (IntSet.elems neighbours) ++ s
  where
    unvisited = IntMap.delete v g
    neighbours = IntMap.findWithDefault IntSet.empty v g

codeGraph :: IntMap Inst -> Graph
codeGraph = (IntMap.map IntSet.singleton) . (IntMap.mapWithKey succInst)

reachable :: IntMap Inst -> IntSet
reachable = (flip reachability [0]) . codeGraph

halting :: IntMap Inst -> IntSet
halting as = reachability (transpose $ codeGraph as) $ IntMap.keys $ IntMap.filter (== Halt) as

-- halting :: IntMap Inst -> IntSet
-- halting m = as
--   where
--     as = IntMap.keysSet $ IntMap.filterWithKey halts $ m
--     halts _ Halt = True
--     halts i (Jmp a) = IntSet.member (i + a) as
--     halts i _ = IntSet.member (succ i) as

-- reachable :: IntMap Inst -> IntSet
-- reachable = simSeen . (execState runHalting) . initSim

fixCode :: IntMap Inst -> IntMap Inst
fixCode is = IntMap.insert a (alt i) is
  where
    rs = reachable is
    hs = halting is
    alt x = case x of
      Jmp a -> Nop a
      Nop a -> Jmp a
      _     -> x
    f addr inst = (IntSet.member addr rs) && (IntSet.member (succInst addr (alt inst)) hs)
    (a, i) = IntMap.findMin $ IntMap.filterWithKey f is

solve :: Case -> Soln
solve = simAcc . (execState runHalting) . initSim . fixCode . instMap
