module Main where

import Data.Bifunctor (bimap)
import Data.Char (isLower)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

data Prop = PropX | PropM | PropA | PropS deriving (Ord, Eq)

data Comp = CompLT | CompGT

data Test = Test Prop Comp Int

data Atom = AtomReject | AtomAccept deriving (Eq)

data Expr = ExprReturn Atom | ExprCall String

data Rule = Rule Test Expr

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

parseTest :: ReadP Test
parseTest = do
  prop <- parseProp
  comp <- parseComp
  num <- parse
  return $ Test prop comp num

parseAtom :: ReadP Atom
parseAtom = choice [char 'R' *> return AtomReject, char 'A' *> return AtomAccept]

parseExpr :: ReadP Expr
parseExpr = (ExprReturn <$> parseAtom) <++ (ExprCall <$> munch1 isLower)

parseRule :: ReadP Rule
parseRule = do
  test <- parseTest
  char ':'
  expr <- parseExpr
  return $ Rule test expr

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
solve (flows, parts) = sum $ map boxVolume accs
  where
    flowm = Map.fromList $ map (\flow@(Flow name _ _) -> (name, flow)) flows
    (accs, rejs) = partitionBoxes flowm [boxDefault]

type Box = Map Prop (Int, Int)

boxDefault :: Box
boxDefault = Map.fromList $ zip [PropX, PropM, PropA, PropS] $ repeat (1, 4001)

boxVolume :: Box -> Int
boxVolume = product . Map.map (uncurry subtract)

boxSplit :: Test -> Box -> (Maybe Box, Maybe Box)
boxSplit (Test prop comp num) plum = (fmap update inlu, fmap update exlu)
  where
    (inlu, exlu) = rangeSplit (comp, num) (plum Map.! prop)
    update lu = Map.insert prop lu plum

rangeSplit :: (Comp, Int) -> (Int, Int) -> (Maybe (Int, Int), Maybe (Int, Int))
rangeSplit (CompLT, m) (l, u) = (if l < m then Just (l, m) else Nothing, if m < u then Just (m, u) else Nothing)
rangeSplit (CompGT, m) (l, u) = swap $ rangeSplit (CompLT, m + 1) (l, u)

partitionBoxes :: Map String Flow -> [Box] -> ([Box], [Box])
partitionBoxes flowm = go "in"
  where
    go name boxes = goFlow (Map.findWithDefault (error name) name flowm) boxes
    goFlow (Flow _ rules final) boxes = (accs ++ faccs, rejs ++ frejs)
      where
        ((accs, rejs), rems) = goRules rules boxes
        (faccs, frejs) = goExpr final rems
    goRules (rule : rules) boxes = ((haccs ++ taccs, hrejs ++ trejs), trems)
      where
        ((haccs, hrejs), hrems) = goRule rule boxes
        ((taccs, trejs), trems) = goRules rules hrems
    goRules [] boxes = (([], []), boxes)
    goRule (Rule test expr) boxes = ((accs, rejs), exs)
      where
        (ins, exs) = bimap catMaybes catMaybes $ unzip $ map (boxSplit test) boxes
        (accs, rejs) = goExpr expr ins
    goExpr expr boxes = case expr of
      ExprReturn AtomAccept -> (boxes, [])
      ExprReturn AtomReject -> ([], boxes)
      ExprCall name -> go name boxes
