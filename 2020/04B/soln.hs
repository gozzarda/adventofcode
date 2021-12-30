import           Data.Char       (isDigit, isHexDigit, isUpper)
import           Data.List       (elemIndex, findIndex, isInfixOf, isPrefixOf,
                                  splitAt, stripPrefix, tails)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe      (fromJust, fromMaybe)

type Passport = Map String String
type Case = [Passport]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = (map parseport) . (splitOn "\n\n")

parseport :: String -> Passport
parseport = Map.fromList . (map parsekeyval) . words

parsekeyval :: String -> (String, String)
parsekeyval s = (k, v)
  where
    i = fromJust $ elemIndex ':' s
    (k, dv) = splitAt i s
    v = tail dv

display :: Soln -> String
display = unlines . singleton . show

singleton :: a -> [a]
singleton x = [x]

validate_int_range :: (Int, Int) -> String -> Bool
validate_int_range (l, u) s = let n = read s :: Int in l <= n && n < u

validate_year :: (Int, Int) -> String -> Bool
validate_year (l, u) s = (length s == 4) && validate_int_range (l, u) s

validate_hgt :: String -> Bool
validate_hgt s = case units of
    "cm" -> validate_int_range (150, 194) value
    "in" -> validate_int_range (59, 77) value
    _    -> False
  where
    value = takeWhile isDigit s
    units = dropWhile isDigit s

validate_hcl :: String -> Bool
validate_hcl s = (head s == '#') && (length hex == 6) && (all isHexDigit hex) && (all (not.isUpper) hex)
  where
    hex = tail s

validate_ecl :: String -> Bool
validate_ecl = flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validate_pid :: String -> Bool
validate_pid s = (length s == 9) && (all isDigit s)

validators :: Map String (String -> Bool)
validators = Map.fromList [
  ("byr", validate_year (1920, 2003)),
  ("iyr", validate_year (2010, 2021)),
  ("eyr", validate_year (2020, 2031)),
  ("hgt", validate_hgt),
  ("hcl", validate_hcl),
  ("ecl", validate_ecl),
  ("pid", validate_pid)]

passport_valid :: Passport -> Bool
passport_valid = (==(Map.size validators)) . (Map.size) . (Map.filter id) . (Map.intersectionWith ($) validators)

solve :: Case -> Soln
solve = length . (filter passport_valid)
