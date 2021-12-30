import           Control.Monad.State.Lazy
import           Data.IntMap              (IntMap)
import qualified Data.IntMap              as IntMap
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IntSet
import           Data.Maybe               (fromJust)

import           Debug.Trace              (traceShowId)

data Inst = Acc Int | Jmp Int | Nop Int
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

initSim :: Case -> Sim
initSim is = Sim {simCode = IntMap.fromList $ zip [0..] is, simSeen = IntSet.empty, simPC = 0, simAcc = 0}

stepSim :: Sim -> Sim
stepSim s = traceShowId s'
  where
    s' = s {simSeen = seen, simPC = pc, simAcc = acc}
    seen = IntSet.insert (simPC s) (simSeen s)
    inst = fromJust $ IntMap.lookup (simPC s) (simCode s)
    pc = case inst of
      Jmp a -> (simPC s) + a
      _     -> (simPC s) + 1
    acc = case inst of
      Acc a -> (simAcc s) + a
      _     -> (simAcc s)

stepSimM :: SimM ()
stepSimM = modify stepSim

runUntilLoop :: SimM ()
runUntilLoop = do
  pc <- gets simPC
  seen <- gets simSeen
  if IntSet.member pc seen
    then return ()
    else stepSimM >> runUntilLoop

solve :: Case -> Soln
solve = simAcc . (execState runUntilLoop) . initSim
