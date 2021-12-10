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
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

flipParen :: Char -> Char
flipParen '(' = ')'
flipParen '[' = ']'
flipParen '{' = '}'
flipParen '<' = '>'

firstIllegalParen :: String -> Maybe Char
firstIllegalParen = go []
  where
    go _ [] = Nothing
    go ls (l:s) | elem l "([{<" = go (l:ls) s
    go (l:ls) (r:s) | r == flipParen l = go ls s
    go _ (r:_) = Just r

solve :: Case -> Soln
solve = sum . map score . catMaybes . map firstIllegalParen
