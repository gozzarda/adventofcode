module Main where

import Data.Char (isDigit)
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (groupBy, sortBy)
import Data.Maybe (fromMaybe)

type Worry = Int

data Term = Old | Const Worry

data Oper = Plus Term Term | Prod Term Term

data Test = Test Worry (Int, Int)

data Monkey = Monkey Int [Worry] Oper Test

type Case = [Monkey]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readMonkey . splitWhen null . lines

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f = filter (not . f . head) . groupBy ((==) `on` f)

readMonkey :: [String] -> Monkey
readMonkey ls = let (idl : itemsl : operl : testls) = ls in Monkey (readID idl) (readWorries itemsl) (readOper operl) (readTest testls)

readID :: String -> Int
readID = read . filter isDigit

readWorries :: String -> [Worry]
readWorries = map read . splitWhen (not . isDigit)

readOper :: String -> Oper
readOper s = op lt rt
  where
    ws = words s
    [lw, ow, rw] = drop (length ws - 3) ws
    op = case ow of
      "+" -> Plus
      "*" -> Prod
    lt = readTerm lw
    rt = readTerm rw

readTerm :: String -> Term
readTerm "old" = Old
readTerm s = Const $ read s

readTest :: [String] -> Test
readTest ls = let [dw, tw, fw] = map (filter isDigit) ls in Test (read dw) (read tw, read fw)

showSoln :: Soln -> String
showSoln = unlines . return . show

type Rule = Worry -> (Int, Worry)

makeRule :: Oper -> Test -> Rule
makeRule oper test old = let new = ((`div` 3) . makeOper oper) old in (makeTest test new, new)

makeOper :: Oper -> Worry -> Worry
makeOper (Plus lt rt) = \old -> makeTerm lt old + makeTerm rt old
makeOper (Prod lt rt) = \old -> makeTerm lt old * makeTerm rt old

makeTerm :: Term -> Worry -> Worry
makeTerm Old = id
makeTerm (Const x) = const x

makeTest :: Test -> Worry -> Int
makeTest (Test d (t, f)) n = if mod n d == 0 then t else f

type Rules = IntMap Rule

makeRules :: [Monkey] -> Rules
makeRules = IntMap.fromList . map (\(Monkey n _ o t) -> (n, makeRule o t))

type State = (IntMap [Worry], IntMap Int)

initState :: [Monkey] -> State
initState ms = (ism, cm)
  where
    ism = IntMap.fromList $ map (\(Monkey n is _ _) -> (n, is)) ms
    cm = IntMap.fromList $ map (\(Monkey n _ _ _) -> (n, 0)) ms

doGame :: Rules -> State -> State
doGame rm s = iterate (doRound rm) s !! 20

doRound :: Rules -> State -> State
doRound rm s@(ism, cm) = foldl (doTurn rm) s $ IntMap.keys ism

doTurn :: Rules -> State -> Int -> State
doTurn rm s@(ism, cm) n = (ism', cm')
  where
    rule = IntMap.findWithDefault (error "No known rule for monkey") n rm
    is = IntMap.findWithDefault [] n ism
    ism' = foldl throw (IntMap.insert n [] ism) is
    throw m i = let (n', i') = rule i in adjustWithDefault [] (i' :) n' m
    cm' = adjustWithDefault 0 (+ length is) n cm

adjustWithDefault :: a -> (a -> a) -> Int -> IntMap a -> IntMap a
adjustWithDefault x f = IntMap.alter (Just . f . fromMaybe x)

statState :: State -> Soln
statState (_, cm) = product $ take 2 $ sortBy (flip compare) $ IntMap.elems cm

solve :: Case -> Soln
solve ms = statState $ doGame (makeRules ms) (initState ms)
