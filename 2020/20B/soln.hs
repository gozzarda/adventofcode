import           Data.List       (group, groupBy, isPrefixOf, sort, tails,
                                  transpose, uncons)
import           Data.List.Split (splitOn)
import           Data.Map        ((!))
import qualified Data.Map        as Map
import           Data.Maybe      (listToMaybe)

import           Debug.Trace

type Case = [(Int, [[Char]])]
type Soln = Int

main :: IO ()
main = interact $ showSoln . solve . readCase

readCase :: String -> Case
readCase s = zip is ts
  where
    ls = lines s
    ps = splitOn [[]] ls
    is = map (read . init . (drop $ length "Tile ") . head) $ ps
    ts = map tail ps

showSoln :: Soln -> String
showSoln = unlines . return . show

type Tile = [[Char]]
type Edge = [Char]

canonize :: Edge -> Edge
canonize xs = if reverse xs < xs then reverse xs else xs

edges :: Tile -> [Edge]
edges xss = map canonize [head xss, map head xss, last xss, map last xss]

rotate :: Tile -> Tile
rotate = transpose . map reverse

rotations :: Tile -> [Tile]
rotations = take 4 . iterate rotate

opts :: Tile -> [Tile]
opts t = (rotations t) ++ (rotations $ transpose t)

fillOut :: Int -> [Tile] -> Tile -> [[Tile]]
fillOut dim ts seed = map (map snd) $ groupBy (\l r -> (fst $ fst l) == (fst $ fst r)) $ Map.assocs grid
  where
    base = Map.fromList $ zip [(r, c) | r <- take dim [0..], c <- take dim [0..]] $ repeat ()
    grid = Map.mapWithKey f base
    f (r, c) _ = g (Map.lookup (pred r, c) grid) (Map.lookup (r, pred c) grid)
    g Nothing Nothing = seed
    g mu ml           = head $ filter (fits mu ml) ts
    fits mu ml t = maybe True (flip fitsu t) mu && maybe True (flip fitsl t) ml
    fitsu u t = (last u == head t) && (head u /= last t)
    fitsl l t = (map last l == map head t) && (map head l /= map last t)

tat :: [a] -> [a]
tat = tail . init

tat2 :: [[a]] -> [[a]]
tat2 = map tat . tat

concat2 :: [[[[a]]]] -> [[a]]
concat2 = concat . (map $ foldr1 $ zipWith (++))

tails2 :: [[a]] -> [[[[a]]]]
tails2 = transpose . map tails . transpose . map tails

monster :: [[Char]]
monster = ["                  # "
          ,"#    ##    ##    ###"
          ," #  #  #  #  #  #   "]

bitmap :: [[Char]] -> [[Bool]]
bitmap = map (map (=='#'))

hasMonster :: Tile -> Bool
hasMonster xss = (bitmap monster) == zipWith (zipWith (&&)) (bitmap monster) (bitmap xss)

numMonsters :: Tile -> Int
numMonsters img = length $ filter id $ map hasMonster $ concat $ tails2 img

solve :: Case -> Soln
solve its = traceShow monsters $ (roughness img) - (monsters * (roughness monster))
  where
    ts = map snd its  -- tiles
    ess = map edges ts  -- tile edges
    cs = Map.fromList $ map (\xs -> (head xs, length xs)) $ group $ sort $ concat ess  -- count of occurrences of each edge
    css = map (map (cs !)) ess  -- occurences per tile edge
    ns = map (length . filter (/=1)) css  -- neighbours per tile
    tls = map fst $ filter ((==2) . snd) $ zip ts ns  -- corners (options for top left)
    tl = head $ filter (isPrefixOf [1, 1] . map (cs !) . edges) $ opts $ head tls  -- top left tile
    ts' = concat $ map opts ts  -- all orientations of all tiles
    dim = round $ traceShowId $ sqrt $ fromIntegral $ length ts  -- dimension of tile grid
    grid = fillOut dim ts' tl -- grid of tiles in image
    img = concat2 $ map (map tat2) grid
    imgs = opts img
    monsters = maximum $ map numMonsters imgs
    roughness = length . filter id . concat . bitmap
