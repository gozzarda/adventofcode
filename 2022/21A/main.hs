{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Char (isAlpha)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

data Expr = Lit Int | Var String | Bin Char Expr Expr

type Case = [(String, Expr)]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map (doReadP' $ parseDefinition <* eof) . lines

doReadP' :: ReadP a -> String -> a
doReadP' p = fromMaybe (error "Parsing failed") . doReadP p

doReadP :: ReadP a -> String -> Maybe a
doReadP p = fmap fst . listToMaybe . readP_to_S p

parse :: Read a => ReadP a
parse = ReadPrec.readPrec_to_P readPrec ReadPrec.minPrec

parseLit :: ReadP Expr
parseLit = Lit <$> parse

parseVar :: ReadP Expr
parseVar = Var <$> munch isAlpha

parseBin :: ReadP Expr
parseBin = do
  l <- munch isAlpha
  skipSpaces
  o <- get
  skipSpaces
  r <- munch isAlpha
  return $ Bin o (Var l) (Var r)

parseExpr :: ReadP Expr
parseExpr = choice [parseLit, parseVar, parseBin]

parseDefinition :: ReadP (String, Expr)
parseDefinition = do
  name <- munch isAlpha
  char ':'
  skipSpaces
  expr <- parseExpr
  return (name, expr)

showSoln :: Soln -> String
showSoln = unlines . return . show

reduce :: Map String Expr -> Map String Expr
reduce m = m'
  where
    m' = M.map go m
    go (Lit x) = Lit x
    go (Var v) = M.findWithDefault (Var v) v m'
    go (Bin o l r) = case (go l, go r) of
      (Lit l', Lit r') -> Lit $ f l' r'
      (l', r') -> Bin o l' r'
      where
        f = case o of
          '+' -> (+)
          '-' -> (-)
          '*' -> (*)
          '/' -> div

fromLit :: Expr -> Int
fromLit (Lit x) = x
fromLit _ = error "fromLit called on non-Lit"

solve :: Case -> Soln
solve ves = fromLit $ reduce (M.fromList ves) ! "root"
