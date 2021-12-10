import           Data.List  (sort)
import           Data.Maybe (catMaybes)

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

score :: Char -> Int
score ')' = 1
score ']' = 2
score '}' = 3
score '>' = 4

flipParen :: Char -> Char
flipParen '(' = ')'
flipParen '[' = ']'
flipParen '{' = '}'
flipParen '<' = '>'

autocompletion :: String -> Maybe String
autocompletion = go []
  where
    go ls [] = Just $ map flipParen ls
    go ls (l:s) | elem l "([{<" = go (l:ls) s
    go (l:ls) (r:s) | r == flipParen l = go ls s
    go _ (r:_) = Nothing

autocompletionScore :: String -> Int
autocompletionScore = sum . zipWith (*) (iterate (* 5) 1) . map score . reverse

-- median, biased up if even
median :: Ord a => [a] -> a
median xs = head $ drop (length xs `div` 2) $ sort xs

solve :: Case -> Soln
solve = median . map autocompletionScore . catMaybes . map autocompletion
