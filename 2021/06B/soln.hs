import qualified Data.IntMap.Strict as IntMap
import           Data.List.Split    (splitWhen)
import           Data.Sequence      (Seq (..), (<|), (|>))
import qualified Data.Sequence      as Seq

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

type Fishtogram = Seq Int

initFishtogram :: [Int] -> Fishtogram
initFishtogram ts = Seq.fromFunction (matureAge + gestation) count
  where
    counts = IntMap.fromListWith (+) $ zip ts $ repeat 1
    count t = IntMap.findWithDefault 0 t counts

stepFishtogram :: Fishtogram -> Fishtogram
stepFishtogram (count :<| counts) = Seq.adjust' (+ count) (pred gestation) $ counts |> count

statFishtogram :: Fishtogram -> Int
statFishtogram = sum

solve :: Case -> Soln
solve = statFishtogram . (!! targetDay) . iterate stepFishtogram . initFishtogram
