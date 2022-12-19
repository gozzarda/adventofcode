module Main where

import Data.Bifunctor
import Data.Bool (bool)
import Data.List (elemIndices, foldl')
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

type Case = [Char]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readCase

readCase :: String -> Case
readCase = filter (\c -> c == '<' || c == '>')

showSoln :: Soln -> String
showSoln = unlines . return . show

rockArts :: [[[Char]]]
rockArts =
  [ [ "####"
    ],
    [ ".#.",
      "###",
      ".#."
    ],
    [ "..#",
      "..#",
      "###"
    ],
    [ "#",
      "#",
      "#",
      "#"
    ],
    [ "##",
      "##"
    ]
  ]

-- (y, x) so that max is top of tower
type Point = (Int, Int)

type Rock = Set Point

rockFromArt :: [[Char]] -> Rock
rockFromArt css = Set.fromList pts
  where
    yxss = map (flip map [0 ..] . (,)) [0 ..]
    yxcss = zipWith zip yxss $ reverse css
    pts = map fst $ filter ((== '#') . snd) $ concat yxcss

rocks :: [Rock]
rocks = map rockFromArt rockArts

type Board = Set Point

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f = uncurry bimap . both f

offsetRock :: Point -> Rock -> Rock
offsetRock d = Set.map (both2 (+) d)

collides :: Point -> Rock -> Board -> Bool
collides d r b = not $ Set.null $ Set.union hits outs
  where
    r' = offsetRock d r
    hits = Set.intersection r' b
    outs = Set.filter (\(y, x) -> x < 0 || 6 < x || y < 0) r'

insert :: Point -> Rock -> Board -> Board
insert d r b = Set.union b $ offsetRock d r

towerHeight :: Board -> Int
towerHeight = maybe 0 ((+ 1) . fst) . Set.lookupMax

type RockState = ([(Int, Char)], Point)

initRockState :: Board -> [(Int, Char)] -> RockState
initRockState b js = (js, (4 + towerHeight b, 2))

stepRockState :: Board -> Rock -> RockState -> RockState
stepRockState b r (j : js, (y, x)) = (js, (y - 1, x''))
  where
    x' = case snd j of '<' -> x - 1; '>' -> x + 1
    x'' = if collides (y - 1, x') r b then x else x'

haltRockState :: Board -> Rock -> RockState -> Bool
haltRockState b r (_, (y, x)) = collides (y - 1, x) r b

statRockState :: Board -> Rock -> RockState -> TowerState
statRockState b r (js, pt) = (js, insert pt r b)

type TowerState = ([(Int, Char)], Board)

initTowerState :: [Char] -> TowerState
initTowerState js = (cycle $ zip [0 ..] js, Set.empty)

stepTowerState :: TowerState -> Rock -> TowerState
stepTowerState (js, b) r = statRockState b r $ until (haltRockState b r) (stepRockState b r) $ initRockState b js

statTowerState :: TowerState -> Int
statTowerState (_, b) = towerHeight b

towerStates :: [Char] -> [TowerState]
towerStates js = scanl stepTowerState (initTowerState js) $ cycle rocks

top :: Int -> Board -> Board
top n b = let h = towerHeight b in Set.map (first $ subtract h) $ snd $ Set.split (h - n, -1) b

eqTowerStates :: TowerState -> TowerState -> Bool
eqTowerStates (ljs, lb) (rjs, rb) = head ljs == head rjs && lb' == rb'
  where
    (hl, hr) = both towerHeight (lb, rb)
    n = min hl hr `div` 2
    (lb', rb') = both (top n) (lb, rb)

evens :: [a] -> [a]
evens (x : _ : xs) = x : evens xs
evens (x : _) = [x]
evens [] = []

solve :: Case -> Soln
solve js = dh * nc + statTowerState (ss !! n')
  where
    n = 1000000000000
    ss = towerStates js
    iths = zip [0 ..] $ zip ss $ evens ss
    ciss = map (fmap fst) $ filter (uncurry eqTowerStates . snd) iths
    (cis, css) = unzip ciss
    chs = map statTowerState css
    (_ : ci : ci' : _) = cis
    di = ci' - ci
    (_ : ch : ch' : _) = chs
    dh = ch' - ch
    (nc, n') = max (0, n) ((+ ci) <$> divMod (n - ci) di)
