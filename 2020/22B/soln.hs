import           Data.Either     (isLeft)
import           Data.Foldable   (toList)
import           Data.List.Split (splitOn)
import           Data.Maybe      (catMaybes)
import           Data.Sequence   (Seq, ViewL (..), (<|), (><), (|>))
import qualified Data.Sequence   as Seq
import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Debug.Trace

type Case = ([Int], [Int])
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSoln . solve . readCase

readCase :: String -> Case
readCase s = (ls, rs)
  where
    (ls:rs:_) = map ((map read) . tail) $ splitOn [""] $ lines s

showSoln :: Soln -> String
showSoln = unlines . return . show

type Deck = Seq Int
type Decks = (Seq Int, Seq Int)
data State = State { curr :: Decks, prevs :: Set Decks } deriving (Show)
type Score = Either Int Int

initState :: Case -> State
initState (ls, rs) = newGame (Seq.fromList ls) (Seq.fromList rs)

newGame :: Deck -> Deck -> State
newGame ls rs = State { curr = (ls, rs), prevs = Set.empty }

step :: State -> State
step state = State { curr = next, prevs = prevs' }
  where
    prevs' = Set.insert (curr state) (prevs state)
    (ls, rs) = curr state
    (l :< ls') = Seq.viewl ls
    (r :< rs') = Seq.viewl rs
    lwins = if l <= Seq.length ls' && r <= Seq.length rs'
      then isLeft $ runGame $ newGame (Seq.take l ls') (Seq.take r rs')
      else l > r
    next = if lwins
      then (ls' |> l |> r, rs')
      else (ls', rs' |> r |> l)

score :: State -> Maybe Score
score state | Set.member (curr state) (prevs state) = Just $ Left $ deckScore $ fst $ curr state
score state | Seq.null $ snd $ curr state = Just $ Left $ deckScore $ fst $ curr state
score state | Seq.null $ fst $ curr state = Just $ Right $ deckScore $ snd $ curr state
score _ = Nothing

deckScore :: Deck -> Int
deckScore ds = sum $ zipWith (*) [1..] $ reverse $ toList ds

runGame :: State -> Score
runGame state = head $ catMaybes $ map score $ iterate step state

solve :: Case -> Soln
solve = either id id . runGame . initState
