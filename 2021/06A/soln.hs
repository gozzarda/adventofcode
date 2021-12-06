import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List.Split    (splitWhen)

type Case = [Int]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase = map read . splitWhen (== ',')

showSoln :: Soln -> String
showSoln = unlines . return . show

birthTime = 7 :: Int
matureAge = 9 :: Int
targetDay = 80 :: Int

-- (Day, (Day + Timer) % birthTime -> Count, (Day + Timer) % matureAge -> Count)
type Fishtogram = (Int, IntMap Int, IntMap Int)

initFishtogram :: [Int] -> Fishtogram
initFishtogram ts = (0, IntMap.fromListWith (+) $ zip ts $ repeat 1, IntMap.empty)

stepFishtogram :: Fishtogram -> Fishtogram
stepFishtogram (d, bs, ms) = (d', bs', ms')
  where
    d' = succ d
    bk = d `mod` birthTime
    mk = d `mod` matureAge
    b = IntMap.findWithDefault 0 bk bs
    m = IntMap.findWithDefault 0 mk ms
    bs' = IntMap.insertWith (+) bk m bs
    ms' = IntMap.insertWith (+) mk b ms

statFishtogram :: Fishtogram -> Int
statFishtogram (_, bs, ms) = (sum $ IntMap.elems bs) + (sum $ IntMap.elems ms)

solve :: Case -> Soln
solve = statFishtogram . (!! targetDay) . iterate stepFishtogram . initFishtogram
