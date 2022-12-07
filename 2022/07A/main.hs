module Main where

import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type Case = [String]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = lines

showSoln :: Soln -> String
showSoln = unlines . return . show

data DirTree = File Int | Dir (Map String DirTree) deriving (Eq, Show)

type Path = [String]

emptyDirTree :: DirTree
emptyDirTree = Dir Map.empty

insertDirTree :: Path -> DirTree -> DirTree -> DirTree
insertDirTree [n] o (Dir m) = Dir $ Map.alter (Just . fromMaybe o) n m
insertDirTree (n : ns) o (Dir m) = Dir $ Map.alter (Just . insertDirTree ns o . fromMaybe emptyDirTree) n m

mkdirDirTree :: Path -> DirTree -> DirTree
mkdirDirTree = flip insertDirTree emptyDirTree

touchDirTree :: Int -> Path -> DirTree -> DirTree
touchDirTree s = flip insertDirTree (File s)

sizesDirTree :: DirTree -> (Int, [Int])
sizesDirTree (File s) = (s, [])
sizesDirTree (Dir m) = let (s, ss) = Map.foldr f (0, []) m in (s, s : ss)
  where
    f t (s, ss) = let (s', ss') = sizesDirTree t in (s + s', ss' ++ ss)

type State = (DirTree, Path)

type Command = [String]

initState :: State
initState = (emptyDirTree, [])

stepState :: State -> Command -> State
stepState (t, p) ["$", "cd", "/"] = (t, [])
stepState (t, p) ["$", "cd", ".."] = (t, init p)
stepState (t, p) ["$", "cd", n] = (t, p ++ [n])
stepState (t, p) ["$", "ls"] = (t, p)
stepState (t, p) ["dir", n] = (mkdirDirTree (p ++ [n]) t, p)
stepState (t, p) [s, n] | all isDigit s = (touchDirTree (read s) (p ++ [n]) t, p)
stepState _ ws = error $ "Unknown command" ++ show ws

statState :: State -> Soln
statState (t, _) = let (_, ss) = sizesDirTree t in sum $ filter (<= 100000) ss

solve :: Case -> Soln
solve = statState . foldl stepState initState . map words
