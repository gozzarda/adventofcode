module Main where

import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq, ViewL ((:<)))
import qualified Data.Sequence as Seq

type Case = [Int]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map read . lines

showSoln :: Soln -> String
showSoln = unlines . return . show

type Index = Int

type Value = Int

type State = Seq (Index, Value)

initState :: [Value] -> State
initState = Seq.mapWithIndex (,) . Seq.fromList

stepState :: State -> Index -> State
stepState s i = s''
  where
    si = fromJust $ Seq.findIndexL ((== i) . fst) s
    iv@(_, v) = Seq.index s si
    s' = Seq.deleteAt si s
    si' = (si + v) `mod` length s'
    s'' = Seq.insertAt si' iv s'

mix :: State -> State
mix s = foldl' stepState s $ take (Seq.length s) [0 ..]

statState :: State -> (Int, Int, Int)
statState s = (x, y, z)
  where
    i = fromJust $ Seq.findIndexL ((== 0) . snd) s
    l = length s
    x = snd $ Seq.index s ((i + 1000) `mod` l)
    y = snd $ Seq.index s ((i + 2000) `mod` l)
    z = snd $ Seq.index s ((i + 3000) `mod` l)

solve :: Case -> Soln
solve vs = x + y + z
  where
    key = 811589153
    vs' = map (* key) vs
    (x, y, z) = statState $ iterate mix (initState vs') !! 10
