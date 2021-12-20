import           Data.Array.Unboxed           (UArray, (!))
import qualified Data.Array.Unboxed           as UArray
import           Data.Bool                    (bool)
import           Data.List                    (tails, transpose)
import           Data.Maybe
import           Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

type Case = ([Bool], [[Bool]])
type Soln = Int

rulesLength = 512 :: Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = fst . fromMaybe (error "Failed to parse case") . listToMaybe . ReadP.readP_to_S parseCase

showSoln :: Soln -> String
showSoln = unlines . return . show

parsePixel :: ReadP Bool
parsePixel = (== '#') <$> ReadP.satisfy (`elem` ".#")

parseCase :: ReadP Case
parseCase = do
  rules <- ReadP.count rulesLength parsePixel
  ReadP.skipSpaces
  image <- ReadP.sepBy1 (ReadP.many1 parsePixel) (ReadP.char '\n')
  ReadP.skipSpaces
  ReadP.eof
  return (rules, image)

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

rulesArray :: [Bool] -> UArray Int Bool
rulesArray = UArray.listArray (0, pred rulesLength)

boolsToInt :: [Bool] -> Int
boolsToInt = sum . zipWith (*) (iterate (2 *) 1) . map fromEnum . reverse

pad :: Int -> a -> [a] -> [a]
pad n x xs = let ps = replicate n x in ps ++ xs ++ ps

pad2 :: Int  -> a -> [[a]] -> [[a]]
pad2 n x = transpose . map (pad n x) . transpose . map (pad n x)

windows :: Int -> [a] -> [[a]]
windows n = foldr (zipWith (:)) (repeat []) . take n . tails

windows2 :: Int -> [[a]] -> [[[[a]]]]
windows2 n = transpose . map (windows n) . transpose . map (windows n)

type Image = [[Bool]]
type State = (Image, Bool) -- (Image, Background)

initState :: Image -> State
initState pss = (pss, False)

stepState :: UArray Int Bool -> State -> State
stepState rs (pss, b) = (pss', b')
  where
    n = 3
    pss' = map (map ((rs !) . boolsToInt . concat)) $ windows2 n $ pad2 (pred n) b pss
    b' = rs ! (boolsToInt $ replicate (n*n) b)

statState :: State -> Int
statState (pss, b) = if b then error "Infinite background is lit" else length $ filter id $ concat pss

solve :: Case -> Soln
solve (rs, pss) = statState $ (!! 2) $ iterate (stepState $ rulesArray rs) $ initState pss
