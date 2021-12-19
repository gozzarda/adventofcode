import           Data.Char                    (isDigit)
import           Data.Either                  (partitionEithers)
import           Data.Maybe
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

data V3 a = V3 a a a deriving (Ord,Eq)

type V3I = V3 Int
type M3I = V3 V3I

type Case = [[V3I]]
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

parseV3 :: ReadP (V3I)
parseV3 = do
  x <- parseInt
  ReadP.char ','
  y <- parseInt
  ReadP.char ','
  z <- parseInt
  return $ V3 x y z

parseScanner :: ReadP [V3I]
parseScanner = do
  ReadP.string "--- scanner "
  parseNat
  ReadP.string " ---"
  ReadP.skipSpaces
  ReadP.sepBy parseV3 ReadP.skipSpaces

parseCase :: ReadP Case
parseCase = (ReadP.sepBy parseScanner ReadP.skipSpaces) <* ReadP.skipSpaces <* ReadP.eof

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

vecSub :: V3I -> V3I -> V3I
vecSub (V3 lx ly lz) (V3 rx ry rz) = V3 (lx - rx) (ly - ry) (lz - rz)

vecAdd :: V3I -> V3I -> V3I
vecAdd (V3 lx ly lz) (V3 rx ry rz) = V3 (lx + rx) (ly + ry) (lz + rz)

vecDot :: V3I -> V3I -> Int
vecDot (V3 lx ly lz) (V3 rx ry rz) = lx * rx + ly * ry + lz * rz

transpose :: M3I -> M3I
transpose (V3 (V3 xx xy xz) (V3 yx yy yz) (V3 zx zy zz)) = (V3 (V3 xx yx zx) (V3 xy yy zy) (V3 xz yz zz))

matMul :: M3I -> M3I -> M3I
matMul l r = V3 x y z
  where
    V3 lx ly lz = l
    V3 rx ry rz = transpose r
    x = V3 (vecDot lx rx) (vecDot lx ry) (vecDot lx rz)
    y = V3 (vecDot ly rx) (vecDot ly ry) (vecDot ly rz)
    z = V3 (vecDot lz rx) (vecDot lz ry) (vecDot lz rz)

matVecMul :: M3I -> V3I -> V3I
matVecMul (V3 x y z) r = V3 (vecDot x r) (vecDot y r) (vecDot z r)

identity :: M3I
identity = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

unitRotations :: [M3I]
unitRotations =
  [ V3 (V3 0 (-1) 0) (V3 1 0 0) (V3 0 0 1) -- XY
  , V3 (V3 0 0 (-1)) (V3 0 1 0) (V3 1 0 0) -- XZ
  , V3 (V3 1 0 0) (V3 0 0 (-1)) (V3 0 1 0) -- YZ
  ]

allRotations :: Set M3I
allRotations = foldl f (Set.singleton identity) unitRotations
  where
    f rs ur = Set.unions $ take 4 $ iterate (Set.map $ matMul ur) rs

type Points = Set V3I

-- The second set offset to match the first if possible
maybeOffset :: Points -> Points -> Maybe Points
maybeOffset ls rs = listToMaybe $ filter ff $ map mf ds
  where
    ds = [ vecSub l r | l <- Set.elems ls, r <- Set.elems rs ]
    mf d = Set.map (vecAdd d) rs
    ff s = (Set.size $ Set.intersection ls s) >= 12

-- The second set transformed to match the first if possible
maybeTransformed :: Set M3I -> Points -> Points -> Maybe Points
maybeTransformed rots ls rs = firstJust $ map (maybeOffset ls) $ map (\m -> Set.map (matVecMul m) rs) $ Set.elems rots

eitherTransformed :: Set M3I -> Points -> Points -> Either Points Points
eitherTransformed rots ls rs = maybe (Left rs) Right $ maybeTransformed rots ls rs

data State = State { fixed :: Points, todo :: [Points], unseen :: [Points] }

initState :: [Points] -> State
initState (v:vs) = State { fixed = Set.empty, todo = [v], unseen = vs }

stepState :: Set M3I -> State -> State
stepState rots state = State { fixed = fixed', todo = todo', unseen = unseen' }
  where
    (u:ts) = todo state
    fixed' = Set.union u $ fixed state
    (unseen',vs) = partitionEithers $ map (eitherTransformed rots u) $ unseen state
    todo' = vs ++ ts

haltState :: State -> Bool
haltState = null . todo

statState :: State -> Int
statState = Set.size . fixed

solve :: Case -> Soln
solve = statState . head . dropWhile (not . haltState) . iterate (stepState allRotations) . initState . map Set.fromList
