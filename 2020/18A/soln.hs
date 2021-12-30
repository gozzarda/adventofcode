import           Control.Applicative (some)
import           Data.Char           (isSpace)
import           Data.Either         (fromRight)
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.String

import           Debug.Trace

type Case = [String]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = lines

showSoln :: Soln -> String
showSoln = unlines . return . show

data Expr = AddE Expr Expr | MulE Expr Expr | IntE Int deriving Show

expr :: Parser Expr
expr = chainl1 term binop

binop :: Parser (Expr -> Expr -> Expr)
binop = AddE <$ char '+' <|> MulE <$ char '*'

term :: Parser Expr
term = IntE . read <$> some digit <|> parens expr

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

parseExpr :: String -> Expr
parseExpr s = let Right e = parse expr "<parseExpr>" $ filter (not . isSpace) s in e

evalExpr :: Expr -> Int
evalExpr (AddE le re) = (+) (evalExpr le) (evalExpr re)
evalExpr (MulE le re) = (*) (evalExpr le) (evalExpr re)
evalExpr (IntE v)     = v

solve :: Case -> Soln
solve ls = traceShow vs $ sum vs
  where
    es = map parseExpr ls
    vs = map evalExpr es
