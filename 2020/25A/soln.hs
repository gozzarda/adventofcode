import           Data.List  (findIndex)
import           Data.Maybe (fromJust)

type Case = (Integer, Integer)
type Soln = Integer

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase s = (x, y)
  where
    (x:y:_) = map read $ lines s

showSoln :: Soln -> String
showSoln = unlines . return . show

mod' :: Integer -> Integer
mod' = flip mod 20201227

step :: Integer -> Integer -> Integer
step subject value = mod' $ subject * value

steps :: Integer -> [Integer]
steps subject = iterate (step subject) 1

solve :: Case -> Soln
solve (l, r) = head $ drop li $ steps r
  where
    li = fromJust $ findIndex (==l) $ steps 7
    -- ri = fromJust $ findIndex (==r) steps
