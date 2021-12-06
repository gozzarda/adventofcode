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

matureAge = 2 :: Int
gestation = 7 :: Int
targetDay = 256 :: Int

-- (day, (day + timer) % (matureAge + gestation) -> population)
type Fishtogram = (Int, IntMap Int)

initFishtogram :: [Int] -> Fishtogram
initFishtogram ts = (0, IntMap.fromListWith (+) $ zip ts $ repeat 1)

stepFishtogram :: Fishtogram -> Fishtogram
stepFishtogram (day, phases) = (succ day, phases')
  where
    phase = flip mod (matureAge + gestation)
    parents = IntMap.findWithDefault 0 (phase day) phases
    phases' = IntMap.insertWith (+) (phase $ day + gestation) parents phases

statFishtogram :: Fishtogram -> Int
statFishtogram = sum . IntMap.elems . snd

solve :: Case -> Soln
solve = statFishtogram . (!! targetDay) . iterate stepFishtogram . initFishtogram
