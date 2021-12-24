import           Control.Monad                (liftM2, liftM3)
import           Data.Bifunctor               (bimap)
import           Data.Char                    (isDigit, toLower)
import           Data.Either
import           Data.Maybe
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

type Imm = Int
data Reg = W | X | Y | Z deriving (Show, Enum, Bounded)
data ReadUOp = Inp deriving (Show, Enum, Bounded)
data ReadIns = ReadR ReadUOp Reg
data PureBOp = Add | Mul | Div | Mod | Eql deriving (Show, Enum, Bounded)
data PureIns = PureRR PureBOp Reg Reg | PureRI PureBOp Reg Imm
data Ins = ReadI ReadIns | PureI PureIns
type Prog = [Ins]

instance Show ReadIns where
  show (ReadR o r) = map toLower $ unwords [show o, show r]

instance Show PureIns where
  show (PureRR o l r) = map toLower $ unwords [show o, show l, show r]
  show (PureRI o l r) = map toLower $ unwords [show o, show l, show r]

instance Show Ins where
  show (ReadI i) = show i
  show (PureI i) = show i

type Case = Prog
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

parseShowLower :: (Show a, Enum a, Bounded a) => ReadP a
parseShowLower = ReadP.choice $ map (\x -> ReadP.string (map toLower $ show x) >> return x) [minBound..maxBound]

parseImm :: ReadP Imm
parseImm = parseInt

parseReg :: ReadP Reg
parseReg = parseShowLower

parseReadUOp :: ReadP ReadUOp
parseReadUOp = parseShowLower

parseReadR :: ReadP ReadIns
parseReadR = liftM2 ReadR parseReadUOp (ReadP.skipSpaces >> parseReg)

parseReadIns :: ReadP ReadIns
parseReadIns = ReadP.choice [parseReadR]

parsePureBOp :: ReadP PureBOp
parsePureBOp = parseShowLower

parsePureRR :: ReadP PureIns
parsePureRR = liftM3 PureRR parsePureBOp (ReadP.skipSpaces >> parseReg) (ReadP.skipSpaces >> parseReg)

parsePureRI :: ReadP PureIns
parsePureRI = liftM3 PureRI parsePureBOp (ReadP.skipSpaces >> parseReg) (ReadP.skipSpaces >> parseImm)

parsePureIns :: ReadP PureIns
parsePureIns = ReadP.choice [parsePureRR, parsePureRI]

parseIns :: ReadP Ins
parseIns = ReadP.choice [ReadI <$> parseReadIns, PureI <$> parsePureIns]

parseProg :: ReadP Prog
parseProg = (ReadP.sepBy parseIns ReadP.skipSpaces) <* ReadP.skipSpaces <* ReadP.eof

parseCase :: ReadP Case
parseCase = parseProg

type Regs = (Int, Int, Int, Int)

regSet :: Reg -> Int -> Regs -> Regs
regSet W w (_, x, y, z) = (w, x, y, z)
regSet X x (w, _, y, z) = (w, x, y, z)
regSet Y y (w, x, _, z) = (w, x, y, z)
regSet Z z (w, x, y, _) = (w, x, y, z)

regGet :: Reg -> Regs -> Int
regGet W (w, _, _, _) = w
regGet X (_, x, _, _) = x
regGet Y (_, _, y, _) = y
regGet Z (_, _, _, z) = z

doReadIns :: ReadIns -> Int -> Regs -> Regs
doReadIns (ReadR Inp r) = regSet r

pureBOp :: PureBOp -> Int -> Int -> Int
pureBOp Add = (+)
pureBOp Mul = (*)
pureBOp Div = quot
pureBOp Mod = rem
pureBOp Eql = (\l r -> if l == r then 1 else 0)

doPureIns :: PureIns -> Regs -> Regs
doPureIns (PureRR o l r) rm = regSet l ((pureBOp o) (regGet l rm) (regGet r rm)) rm
doPureIns (PureRI o l r) rm = regSet l ((pureBOp o) (regGet l rm) r) rm

type Visited = Set (Int, Regs)
type Path = [Int]

dfs :: Prog -> Int -> Regs -> Visited -> Either Visited Path
dfs _ pc rm vs | Set.member (pc, rm) vs = Left vs
dfs [] pc rm vs = if (regGet Z rm) == 0 then Right [] else Left $ Set.insert (pc, rm) vs
dfs ((PureI i):is) pc rm vs = dfs is (succ pc) (doPureIns i rm) vs -- $ Set.insert (pc, rm) vs
dfs ((ReadI i):is) pc rm vs = foldl f (Left $ Set.insert (pc, rm) vs) [1..9]
  where f e d = either (bimap id (d:) . dfs is (succ pc) (doReadIns i d rm)) Right e

digitsToInt :: [Int] -> Int
digitsToInt = sum . zipWith (*) (iterate (* 10) 1) . reverse

solve :: Case -> Soln
solve is = either (error "No valid model number found") digitsToInt $ dfs is 0 (0, 0, 0, 0) Set.empty
