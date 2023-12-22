module Main where

import Data.Char (isLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP

data Type = TypeBroadcaster | TypeFlipFlop | TypeNand deriving Show

type Name = String

type Node = (Type, Name)

type Rule = (Node, [Name])

type Prob = [Rule]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = doReadP' $ parseProb <* eof

doReadP' :: ReadP a -> String -> a
doReadP' p = fromMaybe (error "Parsing failed") . doReadP p

doReadP :: ReadP a -> String -> Maybe a
doReadP p = fmap fst . listToMaybe . readP_to_S p

parseName :: ReadP Name
parseName = munch1 isLower

parseNode :: ReadP Node
parseNode =
  choice
    [ ((,) TypeBroadcaster) <$> parseName,
      ((,) TypeFlipFlop) <$> (char '%' *> parseName),
      ((,) TypeNand) <$> (char '&' *> parseName)
    ]

parseRule :: ReadP Rule
parseRule = do
  node <- parseNode
  skipSpaces
  string "->"
  skipSpaces
  kids <- sepBy1 parseName (char ',' >> skipSpaces)
  return (node, kids)

parseProb :: ReadP Prob
parseProb = sepBy parseRule skipSpaces

solve :: Prob -> Soln
solve rules = statState $ states !! 1000
  where
    adj = Map.fromList [(u, vs) | ((_, u), vs) <- rules]
    states = iterate (stepState adj) (initState adj rules)

type AdjList = Map Name [Name]

adjlistEdges :: AdjList -> [(Name, Name)]
adjlistEdges adj = [(u, v) | (u, vs) <- Map.assocs adj, v <- vs]

adjlistTrans :: AdjList -> AdjList
adjlistTrans = Map.fromListWith (++) . map (fmap return . swap) . adjlistEdges

type Pulse = Bool

data NodeState = StateBroadcaster | StateFlipFlop Pulse | StateNand (Map Name Pulse)

type State = (Map Name NodeState, Map Pulse Int)

initState :: AdjList -> [Rule] -> State
initState adj rules = (state, stats)
  where
    adjt = adjlistTrans adj
    ruleState ((t, name), _) = case t of
      TypeBroadcaster -> (name, StateBroadcaster)
      TypeFlipFlop -> (name, StateFlipFlop False)
      TypeNand -> (name, StateNand $ Map.fromList $ zip (Map.findWithDefault [] name adjt) (repeat False))
    state = Map.fromList $ map ruleState rules
    stats = Map.fromList [(False, 0), (True, 0)]

stepState :: AdjList -> State -> State
stepState adj = go [("button", "broadcaster", False)]
  where
    go [] state = state
    go pulses state = let (state', nexts) = foldl goPulse (state, []) pulses in go (reverse nexts) state'
    goPulse ((state, stats), nexts) (prev, curr, val) = case Map.lookup curr state of
      Nothing -> ((state, stats'), nexts)
      Just (StateBroadcaster) -> ((state, stats'), push val)
      Just (StateFlipFlop s) -> case val of
        True -> ((state, stats'), nexts)
        False -> let s' = not s in ((Map.insert curr (StateFlipFlop s') state, stats'), push s')
      Just (StateNand s) -> let s' = Map.insert prev val s in ((Map.insert curr (StateNand s') state, stats'), push $ (not . and) s')
      where
        stats' = Map.adjust (+ 1) val stats
        push x = (map (\next -> (curr, next, x)) $ adj Map.! curr) ++ nexts

statState :: State -> Int
statState (_, stats) = product stats
