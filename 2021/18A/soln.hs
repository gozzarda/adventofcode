import           Data.Char                    (isDigit)
import           Data.Function                ((&))
import           Data.Maybe
import           Text.ParserCombinators.ReadP (ReadP, (+++))
import qualified Text.ParserCombinators.ReadP as ReadP

-- SnailfishNumber (SFN)
data SFN = Leaf Int | Pair SFN SFN

instance Show SFN where
  show (Leaf v)     = show v
  show (Pair lc rc) = "[" ++ (show lc) ++ "," ++ (show rc) ++ "]"

type Case = [SFN]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = fst . fromMaybe (error "Failed to parse case") . listToMaybe . ReadP.readP_to_S parseCase

showSoln :: Soln -> String
showSoln = unlines . return . show

parseNat :: ReadP Int
parseNat = read <$> (ReadP.many1 $ ReadP.satisfy isDigit)

parseEither :: ReadP a -> ReadP b -> ReadP (Either a b)
parseEither lp rp = (Left <$> lp) +++ (Right <$> rp)

parseLeaf :: ReadP SFN
parseLeaf = Leaf <$> parseNat

parsePair :: ReadP SFN
parsePair = do
  ReadP.char '['
  lc <- parseSFN
  ReadP.char ','
  rc <- parseSFN
  ReadP.char ']'
  return $ Pair lc rc

parseSFN :: ReadP SFN
parseSFN = parseLeaf +++ parsePair

parseCase :: ReadP Case
parseCase = ReadP.many (parseSFN <* ReadP.skipSpaces) <* ReadP.eof

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

addLmost :: SFN -> Int -> SFN
addLmost (Leaf v) n     = Leaf $ v + n
addLmost (Pair lc rc) n = Pair (addLmost lc n) rc

addRmost :: SFN -> Int -> SFN
addRmost (Leaf v) n     = Leaf $ v + n
addRmost (Pair lc rc) n = Pair lc (addRmost rc n)

explode' :: Int -> SFN -> Maybe (SFN, (Maybe Int, Maybe Int))
explode' _ (Leaf _) = Nothing
explode' d _ | d < 0 = error "Illegal state before exploding"
explode' 0 (Pair (Leaf lv) (Leaf rv)) = Just (Leaf 0, (Just lv, Just rv))
explode' d (Pair lc rc) = firstJust [el, er]
  where
    el = fmap (\(lc', (ml, mr)) -> (Pair lc' (maybe rc (addLmost rc) mr), (ml, Nothing))) $ explode' (pred d) lc
    er = fmap (\(rc', (ml, mr)) -> (Pair (maybe lc (addRmost lc) ml) rc', (Nothing, mr))) $ explode' (pred d) rc

explodeMaybe :: SFN -> Maybe SFN
explodeMaybe = fmap fst . explode' 4

splitMaybe :: SFN -> Maybe SFN
splitMaybe (Leaf v) | v < 10 = Nothing
splitMaybe (Leaf v) = let l = div v 2 in Just $ Pair (Leaf l) (Leaf $ v - l)
splitMaybe (Pair lc rc) = firstJust [sl, sr]
  where
    sl = fmap (\lc' -> Pair lc' rc) $ splitMaybe lc
    sr = fmap (\rc' -> Pair lc rc') $ splitMaybe rc

reduceMaybe :: SFN -> Maybe SFN
reduceMaybe sfn = firstJust $ map (sfn &) [explodeMaybe, splitMaybe]

reduce :: SFN -> SFN
reduce = fromJust . last . takeWhile isJust . iterate (reduceMaybe . fromJust) . Just

addition :: SFN -> SFN -> SFN
addition l r = reduce $ Pair l r

magnitude :: SFN -> Int
magnitude (Leaf v)     = v
magnitude (Pair lc rc) = (3 * magnitude lc) + (2 * magnitude rc)

solve :: Case -> Soln
solve = magnitude . foldl1 addition
