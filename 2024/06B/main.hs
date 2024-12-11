module Main where

import Data.Array
import Data.Bifunctor
import Data.Either
import Data.Function (on)
import Data.List (findIndices, unfoldr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Debug.Trace

type Prob = [[Char]]

type Soln = Int

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . show

readProb :: String -> Prob
readProb = lines

type Vec2 = (Int, Int)

dirs :: [Vec2]
dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]

both :: (a -> b) -> (a, a) -> (b, b)
both f (l, r) = (f l, f r)

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (lf, ls) (rf, rs) = (f lf rf, f ls rs)

-- Construct functional graph
-- Turn it into a forest, handling cycles
--   As detours?
--   By compression?
-- Precompute DFS times from tree roots (for ancestor check)
-- Mark every state with its root
-- Trace original path for bit of speedup?
-- For each obstacle location along original path
--   Figure out what edges change and overlay detours
--   Start at start state and walk until hit root or cycle by repeatedly:
--     Check every detour for if it is an ancestor of current vertex and if so find lowest
--       Is ancestor if start and finish times bracket ours
--       Is lower if start time is greater
--     Follow lowest ancestor detour, or skip to root and end if none
--   If walk finds a cycle, count it
-- Return count of obstacles that caused cycles

solve :: Prob -> Soln
solve css = length $ filter (not . obstacleEscapes) obstacles
  where
    -- Construct grid
    nr = length css
    nc = maximum $ map length css
    rcBounds = ((0, 0), (nr - 1, nc - 1))
    grid = listArray rcBounds $ concat css
    get rc = if inRange rcBounds rc then Just (grid ! rc) else Nothing
    -- Construct state function, etc.
    nextState (Just (rc, drc@(dr, dc))) =
      let rc' = both2 (+) rc drc
       in case get rc' of
            Just '#' -> Just (rc, (dc, -dr))
            Just _ -> Just (rc', drc)
            _ -> Nothing
    nextState Nothing = Nothing
    initCoord = fst $ head $ filter ((== '^') . snd) $ assocs grid
    initState = Just (initCoord, (-1, 0))
    path = map fst $ catMaybes $ takeWhile isJust $ iterate nextState initState
    states = Nothing : [Just (rc, drc) | rc <- indices grid, drc <- dirs]
    -- Construct forest rooted at arbitrary vertex from each cycle
    cycles = findCycles nextState $ Set.fromList states
    roots = mapMaybe listToMaybe cycles
    g = graphFromFunc nextState states
    gt = transposeGraph g
    -- Construct new state function that skips to next detour up tree
    vdfm = dfsTimes gt roots
    dvm = Map.fromList $ map (swap . fmap fst) $ Map.assocs vdfm
    nextDetour duvm u = case Map.lookup u duvm of
      Just v -> v
      Nothing -> dvm Map.! maximum ads -- Lowest ancestor in duvm
      where
        (ud, uf) = vdfm Map.! u
        ddfs = Map.elems $ Map.intersection vdfm duvm
        ads = map fst $ filter (\(d, f) -> d <= ud && uf <= f) ddfs
    -- Detours for roots and some obstacle location
    rootDetours = [(root, nextState root) | root <- roots]
    obstacleDetours (r, c) = map f dirs
      where
        f (dr, dc) =
          let rc' = (r - dr, c - dc)
           in (Just (rc', (dr, dc)), Just (rc', (dc, -dr)))
    obstacleEscapes rc = Set.member Nothing seen
      where
        duvm = Map.fromList $ rootDetours ++ obstacleDetours rc
        seen = iterateSet (nextDetour duvm) initState
    obstacles = Set.elems $ Set.delete initCoord $ Set.fromList path

-- Finds the set of f-cycles that all xs get stuck in
findCycles :: (Ord a) => (a -> a) -> Set a -> [[a]]
findCycles f = go
  where
    go unseen = case Set.lookupMin unseen of
      Nothing -> []
      Just x ->
        let (pref, cycl) = findCycle (`Set.member` unseen) f x
            cycls = go (Set.difference unseen $ Set.fromList $ pref ++ cycl)
         in if null cycl then cycls else cycl : cycls

-- Stops when predicate fails, returns (prefix, cycle) if cycles
findCycle :: (Eq a) => (a -> Bool) -> (a -> a) -> a -> ([a], [a])
findCycle p f x =
  let ts = takeWhile p $ iterate f x -- Tortoise
      hs = odds ts -- Hare
   in case findIndices (uncurry (==)) $ zip ts hs of
        (i : i' : _) ->
          let n = i' - i
              (pref, suff) = span (uncurry (/=)) $ zip ts $ drop n ts
           in (map fst pref, take n $ map fst suff)
        _ -> (ts, [])

odds :: [a] -> [a]
odds (x : xs) = evens xs
odds [] = []

evens :: [a] -> [a]
evens (x : xs) = x : odds xs
evens [] = []

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f (x : xs) = x : if f x then [] else takeUntil f xs
takeUntil _ [] = []

-- Set of all values reached by `iterate f x`
iterateSet :: (Ord a) => (a -> a) -> a -> Set a
iterateSet f x = go Set.empty x
  where
    go seen x | Set.member x seen = seen
    go seen x = go (Set.insert x seen) (f x)

type Vert = Maybe (Vec2, Vec2)

type Edge = (Vert, Vert)

type Graph = Map Vert (Set Vert)

graphFromEdges :: [Edge] -> Graph
graphFromEdges = Map.fromListWith Set.union . map (fmap Set.singleton)

edgesFromGraph :: Graph -> [Edge]
edgesFromGraph g = [(u, v) | (u, vs) <- Map.assocs g, v <- Set.elems vs]

graphFromFunc :: (Vert -> Vert) -> [Vert] -> Graph
graphFromFunc f vs = graphFromEdges $ zip vs $ map f vs

transposeGraph :: Graph -> Graph
transposeGraph = graphFromEdges . map swap . edgesFromGraph

-- Left is discover, Right is finished
dfsOrder :: Graph -> [Vert] -> [Either Vert Vert]
dfsOrder g roots = reverse $ snd $ foldl go (Set.empty, []) roots
  where
    go (seen, dro) u | Set.member u seen = (seen, dro)
    go (seen, dro) u =
      let vs = Map.findWithDefault Set.empty u g
          sd = (Set.insert u seen, Left u : dro)
          (seen', dro') = Set.foldl go sd vs
       in (seen', Right u : dro')

dfsTimes :: Graph -> [Vert] -> Map Vert (Int, Int)
dfsTimes g roots = Map.intersectionWith (,) dvim fvim
  where
    ivs = zipWith (\i -> bimap (,i) (,i)) [0 ..] (dfsOrder g roots)
    (dvis, fvis) = partitionEithers ivs
    dvim :: Map Vert Int
    dvim = Map.fromList dvis
    fvim :: Map Vert Int
    fvim = Map.fromList fvis
