type Case = [String]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = lines

display :: Soln -> String
display = unlines . singleton . show

singleton :: a -> [a]
singleton x = [x]

decodeSeat :: String -> Int
decodeSeat = sum . (map fst) . (filter snd) . (zip $ iterate (*2) 1) . reverse . (map f)
  where
    f 'F' = False
    f 'B' = True
    f 'L' = False
    f 'R' = True

solve :: Case -> Soln
solve = maximum . (map decodeSeat)
