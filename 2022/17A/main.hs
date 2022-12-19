module Main where

import Data.Bool (bool)
import Data.List (foldl')
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

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (ll, lr) (rl, rr) = (f ll rl, f lr rr)

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

type RockState = ([Char], Point)

initRockState :: Board -> [Char] -> RockState
initRockState b js = (js, (4 + towerHeight b, 2))

stepRockState :: Board -> Rock -> RockState -> RockState
stepRockState b r (j : js, (y, x)) = (js, (y - 1, x''))
  where
    x' = case j of '<' -> x - 1; '>' -> x + 1
    x'' = if collides (y - 1, x') r b then x else x'

haltRockState :: Board -> Rock -> RockState -> Bool
haltRockState b r (_, (y, x)) = collides (y - 1, x) r b

statRockState :: Board -> Rock -> RockState -> TowerState
statRockState b r (js, pt) = (js, insert pt r b)

type TowerState = ([Char], Board)

initTowerState :: [Char] -> TowerState
initTowerState js = (cycle js, Set.empty)

stepTowerState :: TowerState -> Rock -> TowerState
stepTowerState (js, b) r = statRockState b r $ until (haltRockState b r) (stepRockState b r) $ initRockState b js

statTowerState :: TowerState -> Int
statTowerState (_, b) = towerHeight b

solve :: Case -> Soln
solve js = statTowerState $ foldl' stepTowerState (initTowerState js) rs
  where
    n = 2022
    rs = take n $ cycle rocks
