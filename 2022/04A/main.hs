module Main where

type Range = (Int, Int)

type Case = [(Range, Range)]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = map readPair . lines

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy f = fmap (dropWhile f) . break f

both :: (a -> b) -> (a, a) -> (b, b)
both f (l, r) = (f l, f r)

readPair :: String -> (Range, Range)
readPair = both readRange . splitBy (== ',')

readRange :: String -> Range
readRange = both read . splitBy (== '-')

showSoln :: Soln -> String
showSoln = unlines . return . show

solve :: Case -> Soln
solve = length . filter (uncurry eitherContains)

eitherContains :: Range -> Range -> Bool
eitherContains l r = rangeContains l r || rangeContains r l

rangeContains :: Range -> Range -> Bool
rangeContains (ll, lu) (rl, ru) = ll <= rl && ru <= lu
