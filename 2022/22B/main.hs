module Main where

import Control.Applicative (liftA2)
import Data.Bifunctor (bimap)
import Data.Char
import Data.Foldable (find)
import Data.Function (on)
import Data.List (foldl', groupBy, transpose)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe

data Move = Walk Int | Turn Char

type Case = ([[Char]], [Move])

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase s = let ls = lines s in (init $ init ls, readMoves $ last ls)

readMoves :: String -> [Move]
readMoves = map readMove . groupBy ((==) `on` isNumber)

readMove :: String -> Move
readMove s | all isDigit s = Walk $ read s
readMove [c] = Turn c

showSoln :: Soln -> String
showSoln = unlines . return . show

data Dir = R | D | L | U deriving (Eq, Ord, Enum, Bounded, Show)

turn :: Char -> Dir -> Dir
turn 'L' R = U
turn 'L' d = pred d
turn 'R' U = R
turn 'R' d = succ d
turn 'U' d = turn 'R' $ turn 'R' d

type Point = (Int, Int)

offset :: Dir -> Point -> Point
offset R (r, c) = (r, c + 1)
offset D (r, c) = (r + 1, c)
offset L (r, c) = (r, c - 1)
offset U (r, c) = (r - 1, c)

pointToEdgeInd :: Int -> Dir -> Point -> Int
pointToEdgeInd _ R (r, _) = r
pointToEdgeInd l D (_, c) = l - 1 - c
pointToEdgeInd l L (r, _) = l - 1 - r
pointToEdgeInd _ U (_, c) = c

edgeIndToPoint :: Int -> Dir -> Int -> Point
edgeIndToPoint l R i = (i, l - 1)
edgeIndToPoint l D i = (l - 1, l - 1 - i)
edgeIndToPoint l L i = (l - 1 - i, 0)
edgeIndToPoint _ U i = (0, i)

crossEdge :: Int -> Dir -> Dir -> Point -> Point
crossEdge l d d' = edgeIndToPoint l d' . ((l - 1) -) . pointToEdgeInd l d

type FaceID = (Int, Int)

type Edge = (FaceID, Dir)

type Net = Map Edge Edge

foldCorner :: Net -> Edge -> Net
foldCorner m o = fromMaybe id mf m
  where
    o' = fmap (turn 'R') o
    me = fmap (turn 'L') <$> M.lookup o m
    me' = fmap (turn 'R') <$> M.lookup o' m
    mf = liftA2 (\e e' -> M.insert e e' . M.insert e' e) me me'

foldNet :: Net -> Net
foldNet m = if M.size m' /= 24 then error "foldNet failed" else m'
  where
    m' = iterate f m !! (12 - 5)
    f m = foldl' foldCorner m $ M.keys m

makeNet :: [FaceID] -> Net
makeNet is = foldNet $ M.fromList $ filter (\(_, (i, _)) -> i `elem` is) ps
  where
    ps = concatMap (flip map is . f) [minBound .. maxBound]
    f d i = ((i, d), (offset d i, turn 'U' d))

type Face = Map Point Char

makeFace :: [[Char]] -> Face
makeFace xss = M.fromList [((r, c), x) | (r, xs) <- zip [0 ..] xss, (c, x) <- zip [0 ..] xs]

type Cube = (Int, Map FaceID Face, Net)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (c, xs') = splitAt n xs in c : chunksOf n xs'

makeCube :: [[Char]] -> Cube
makeCube xss = (l, M.fromList ifs, makeNet is)
  where
    l = minimum $ map (minimum . map length . groupBy ((==) `on` isSpace)) (xss ++ transpose xss)
    w = maximum $ map length xss
    fss = transpose $ map (chunksOf l) $ transpose $ map (chunksOf l . take w . (++ repeat ' ')) xss
    ifs = [((r, c), makeFace f) | (r, fs) <- zip [0 ..] fss, (c, f) <- zip [0 ..] fs, not $ isSpace $ head $ head f]
    (is, _) = unzip ifs

cubeLookup :: (FaceID, Point) -> Cube -> Maybe Char
cubeLookup (i, p) (_, fm, _) = M.lookup i fm >>= M.lookup p

type State = (FaceID, Point, Dir)

next :: Cube -> State -> (State, Char)
next cube@(l, fm, em) (i, p, d) = head $ catMaybes [msx, msx']
  where
    p' = offset d p
    (i', d') = em ! (i, d)
    p'' = crossEdge l d d' p'
    d'' = turn 'U' d'
    msx = ((i, p', d),) <$> cubeLookup (i, p') cube
    msx' = ((i', p'', d''),) <$> cubeLookup (i', p'') cube

step :: Cube -> State -> Maybe State
step cube s = let (s', x) = next cube s in if x == '#' then Nothing else Just s'

walk :: Cube -> State -> Int -> State
walk _ s 0 = s
walk cube s n = maybe s (\s -> walk cube s (n - 1)) (step cube s)

move :: Cube -> State -> Move -> State
move _ (i, p, d) (Turn c) = (i, p, turn c d)
move cube s (Walk n) = walk cube s n

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f = uncurry bimap . both f

stat :: Cube -> State -> Int
stat (l, fm, _) (i, p, d) = 1000 * r + 4 * c + fromEnum d
  where
    (r, c) = both (+ 1) $ both2 (+) p $ both (* l) i

solve :: Case -> Soln
solve (grid, moves) = stat cube $ foldl' (move cube) (i, p, d) moves
  where
    cube = makeCube grid
    (_, fm, _) = cube
    (i, f) = M.findMin fm
    (p, _) = M.findMin f
    d = R
