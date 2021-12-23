import           Control.Applicative      (liftA2)
import           Control.Monad            (forM)
import           Control.Monad.State.Lazy
import           Data.Char                (isLetter)
import           Data.Either              (fromLeft, lefts)
import           Data.List                (dropWhileEnd, intersperse, transpose)
import           Data.Map.Strict          (Map, (!))
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (catMaybes, fromJust)
import qualified Data.Set                 as Set

data Colour = A | B | C | D deriving (Ord,Eq,Show,Read)
type Case = [[Colour]]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map (map (read . (:[]))) . filter (not . null) . map (filter isLetter) . transpose . lines

showSoln :: Soln -> String
showSoln = unlines . return . show

type Hall = Map Int Colour
type Room = Either [Colour] Int
type DPK = (Hall, Map Colour Room)
type DPV = Maybe Int
type DPT = Map DPK DPV

hallLwr = 0 :: Int
hallUpr = 10 :: Int

distCosts :: Map Colour Int
distCosts = Map.fromList [(A, 1), (B, 10), (C, 100), (D, 1000)]

roomInds :: Map Colour Int
roomInds = Map.fromList [(A, 2), (B, 4), (C, 6), (D, 8)]

halldist :: Hall -> Int -> Int -> Maybe Int
halldist m s d = if passable then Just $ abs $ s - d else Nothing
  where passable = maybe True (>= (max s d)) $ Set.lookupGT (min s d) $ Map.keysSet m

enters :: Map Colour Int -> DPK -> [(DPK, Int)]
enters ds (hm, rm) = catMaybes $ map (uncurry f) $ Map.assocs hm
  where
    f i c = fmap ((,) $ move i c) $ cost i c
    move i c = (Map.delete i hm, Map.adjust (either undefined (Right . succ)) c rm)
    cost i c = fmap ((*) $ distCosts ! c) $ dist i c
    dist i c = liftA2 (+) (roomdist c) (halldist hm i $ roomInds ! c)
    roomdist c = either (const Nothing) (Just . (-) (ds ! c)) =<< Map.lookup c rm

leaves :: Map Colour Int -> DPK -> [(DPK, Int)]
leaves ds (hm, rm) = catMaybes $ [ f r i | r <- Map.keys rm, i <- maybe [] snd $ Map.lookupLE (roomInds ! r) ism ]
  where
    f r i = fmap ((,) $ move r i) $ cost r i
    ism = Map.mapWithKey (\l u -> filter (not . (`elem` (Map.elems roomInds))) [l..u]) lum
    lum = let is = Map.keys hm in Map.fromList $ zip (hallLwr : map succ is) (map pred is ++ [hallUpr])
    move r i = let (c:cs) = fromLeft undefined (rm ! r) in (Map.insert i c hm, Map.insert r (if null cs then Right 0 else Left cs) rm)
    cost r i = let c = head $ fromLeft undefined (rm ! r) in fmap ((*) $ distCosts ! c) $ dist r i
    dist r i = liftA2 (+) (roomdist r) (halldist hm i $ roomInds ! r)
    roomdist r = either (Just . (-) (ds ! r) . length . tail) (const Nothing) =<< Map.lookup r rm

neighbours :: Map Colour Int -> DPK -> [(DPK, Int)]
neighbours ds k = if null es then ls else es
  where
    es = enters ds k
    ls = leaves ds k

cached :: (DPK -> State DPT DPV) -> DPK -> State DPT DPV
cached f k = do
  v <- f k
  modify $ Map.insert k v
  return v

mdpf :: Map Colour Int -> DPK -> State DPT DPV
mdpf ds k = do
  mv <- gets $ Map.lookup k
  maybe (cached (dpf ds) k) return mv

dpf :: Map Colour Int -> DPK -> State DPT DPV
dpf ds k = if haltKey k then return $ Just 0 else do
  let (ns, ws) = unzip $ neighbours ds k
  cs <- catMaybes <$> zipWith (fmap . (+)) ws <$> forM ns (mdpf ds)
  return $ minimum' cs

minimum' :: (Ord a) => [a] -> Maybe a
minimum' [] = Nothing
minimum' xs = Just $ minimum xs

haltKey :: DPK -> Bool
haltKey (hm, rm) = (Map.null hm) && (null $ lefts $ Map.elems rm)

roomsToKey :: [[Colour]] -> DPK
roomsToKey rss = (Map.empty, Map.fromList $ zip [A, B, C, D] $ map (\rs -> if null rs then Right 0 else Left rs) rss)

showBoard :: Map Colour Int -> DPK -> String
showBoard ds (hm, rm) = unlines $ (hs:) $ transpose $ replicate 2 bs ++ intersperse bs rss
  where
    hs = concat $ map (flip (Map.findWithDefault ".") $ Map.map show hm) [hallLwr..hallUpr]
    rss = map (pad . (uncurry $ showRoom ds)) $ Map.assocs rm
    pad s = s ++ (replicate ((maximum $ Map.elems ds) - length s) ' ')
    bs = pad []

showRoom :: Map Colour Int -> Colour -> Room -> String
showRoom ds c r = (\s -> replicate ((ds ! c) - length s) '.' ++ s) $ either (concat . map show) (flip replicate '*') r

solve :: Case -> Soln
solve rss = fromJust $ evalState (dpf ds $ roomsToKey rss') Map.empty
  where
    rss' = zipWith (dropWhileEnd . (==)) [A, B, C, D] rss
    ds = Map.fromList $ zip [A, B, C, D] $ map length rss'
