import           Data.List  (findIndex, isInfixOf, isPrefixOf, stripPrefix,
                             tails)
import           Data.Maybe (fromMaybe)

type Case = [String]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = splitOn "\n\n"

splitOn :: String -> String -> [String]
splitOn d s = (p:ps)
  where
    i = findIndex (d `isPrefixOf`) (tails s)
    p = maybe s (flip take s) i
    s' = fmap ((stripPrefix' d) . (flip drop s)) i
    ps = maybe [] (splitOn d) s'

stripPrefix' :: String -> String -> String
stripPrefix' p s = fromMaybe s (stripPrefix p s)

display :: Soln -> String
display = unlines . return . show

singleton :: a -> [a]
singleton x = [x]

passport_valid :: String -> Bool
passport_valid s = all (`isInfixOf` s) ["byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:"]

solve :: Case -> Soln
solve = length . (filter passport_valid)
