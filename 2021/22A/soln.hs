import           Data.Char                    (isDigit)
import           Data.Function                ((&))
import           Data.List                    (sort)
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Text.ParserCombinators.ReadP (ReadP, (+++))
import qualified Text.ParserCombinators.ReadP as ReadP

type Range = (Int, Int)
type Cuboid = [Range]
type Command = (Bool, Cuboid)
type Case = [Command]
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

parseInt :: ReadP Int
parseInt = do
  (s, _) <- ReadP.gather $ (ReadP.optional $ ReadP.char '-') >> parseNat
  return $ read s

parseRange :: ReadP Range
parseRange = do
  ReadP.get
  ReadP.char '='
  lwr <- parseInt
  ReadP.string ".."
  upr <- parseInt
  return (lwr, upr)

parseCuboid :: ReadP Cuboid
parseCuboid = ReadP.sepBy1 parseRange $ ReadP.char ','

parseCommand :: ReadP Command
parseCommand = do
  o <- (ReadP.string "on" >> return True) +++ (ReadP.string "off" >> return False)
  ReadP.skipSpaces
  c <- parseCuboid
  return (o, c)

parseCase :: ReadP Case
parseCase = (ReadP.sepBy parseCommand ReadP.skipSpaces) <* ReadP.skipSpaces <* ReadP.eof

type Priority = Int
type PCommand = (Priority, Command)
type Event = (Int, Set PCommand -> Set PCommand)

split :: PCommand -> [Event]
split (p, (b, (l, u):rs)) = let c = (p, (b, rs)) in [(l, Set.insert c), (succ u, Set.delete c)]

coverage :: Set PCommand -> Int
coverage pcs | Set.null pcs = 0
coverage pcs | any (null . snd . snd) (Set.elems pcs) = maybe 0 (fromEnum . fst . snd) $ Set.lookupMax pcs
coverage pcs = sum $ zipWith (*) cs ds
  where
    (ts, fs) = unzip $ Map.assocs $ Map.fromListWith (.) $ concat $ map split $ Set.elems pcs
    cs = map coverage $ tail $ scanl (&) Set.empty fs
    ds = zipWith subtract ts $ tail ts

solve :: Case -> Soln
solve = coverage . Set.fromList . zip [0..] . filter f
  where
    f (_, rs) = (<= 50) $ maximum $ map (\(l, u) -> max (abs l) (abs u)) rs
