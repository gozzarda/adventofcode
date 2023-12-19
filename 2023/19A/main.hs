module Main where

import Data.Char (isLower)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

data Prop = PropX | PropM | PropA | PropS deriving (Ord, Eq)

data Comp = CompLT | CompGT

data Atom = AtomReject | AtomAccept deriving (Eq)

data Expr = ExprReturn Atom | ExprCall String

data Rule = Rule Prop Comp Int Expr

data Flow = Flow String [Rule] Expr

data Part = Part (Map Prop Int)

type Prob = ([Flow], [Part])

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

-- I have decided I am not playing around with parsing this nonsense by nested splits.
-- Time to break out the parser combinators.
readProb :: String -> Prob
readProb = doReadP' $ parseProb <* eof

doReadP' :: ReadP a -> String -> a
doReadP' p = fromMaybe (error "Parsing failed") . doReadP p

doReadP :: ReadP a -> String -> Maybe a
doReadP p = fmap fst . listToMaybe . readP_to_S p

parse :: Read a => ReadP a
parse = ReadPrec.readPrec_to_P readPrec ReadPrec.minPrec

parseProp :: ReadP Prop
parseProp = choice [char 'x' *> return PropX, char 'm' *> return PropM, char 'a' *> return PropA, char 's' *> return PropS]

parseComp :: ReadP Comp
parseComp = choice [char '<' *> return CompLT, char '>' *> return CompGT]

parseAtom :: ReadP Atom
parseAtom = choice [char 'R' *> return AtomReject, char 'A' *> return AtomAccept]

parseExpr :: ReadP Expr
parseExpr = (ExprReturn <$> parseAtom) <++ (ExprCall <$> munch1 isLower)

parseRule :: ReadP Rule
parseRule = do
  prop <- parseProp
  comp <- parseComp
  num <- parse
  char ':'
  expr <- parseExpr
  return $ Rule prop comp num expr

parseFlow :: ReadP Flow
parseFlow = do
  name <- munch1 isLower
  char '{'
  rules <- endBy parseRule (char ',')
  final <- parseExpr
  char '}'
  return $ Flow name rules final

parsePart :: ReadP Part
parsePart = do
  char '{'
  kvs <-
    sepBy
      ( do
          k <- parseProp
          char '='
          v <- parse
          return (k, v)
      )
      (char ',')
  char '}'
  return $ Part $ Map.fromList kvs

parseProb :: ReadP Prob
parseProb = do
  flows <- endBy parseFlow skipSpaces
  parts <- endBy parsePart skipSpaces
  return (flows, parts)

solve :: Prob -> Soln
solve (flows, parts) = sum $ map (\(Part kvm) -> sum kvm) accepted
  where
    flowm = Map.fromList $ map (\flow@(Flow name _ _) -> (name, flow)) flows
    accepted = filter ((== AtomAccept) . eval flowm) parts

eval :: Map String Flow -> Part -> Atom
eval flowm part = go "in"
  where
    go name = case evalFlow (Map.findWithDefault (error name) name flowm) part of
      ExprCall callee -> go callee
      ExprReturn atom -> atom

evalFlow :: Flow -> Part -> Expr
evalFlow (Flow _ rules final) part = fromMaybe final $ evalRules rules part

evalRules :: [Rule] -> Part -> Maybe Expr
evalRules rules part = firstJust $ map (\rule -> evalRule rule part) rules

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

evalRule :: Rule -> Part -> Maybe Expr
evalRule (Rule prop comp num expr) part = if (compFunc comp) (partProp prop part) num then Just expr else Nothing

compFunc :: Comp -> (Int -> Int -> Bool)
compFunc CompLT = (<)
compFunc CompGT = (>)

partProp :: Prop -> Part -> Int
partProp prop (Part kvm) = Map.findWithDefault 0 prop kvm
