import           Data.Bits
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List   (foldr1, isPrefixOf, (\\))
import           Data.Maybe  (catMaybes, fromJust)
import           Data.Word

import           Debug.Trace

data Inst = Mask [Maybe Bool] | Mem Int Word64
type Case = [Inst]
type Soln = Word64

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse = map parseInst . lines

parseInst :: String -> Inst
parseInst s = if "mask" `isPrefixOf` s then parseMask s else parseMem s
  where
    parseMask = Mask . map f . reverse . last . words
      where
        f '0' = Just False
        f '1' = Just True
        f 'X' = Nothing
    parseMem s = Mem a d
      where
        (as:ds:_) = words $ s \\ "mem[] ="
        a = read as
        d = read ds

display :: Soln -> String
display = unlines . return . show

putBit :: (Bits a) => Int -> Bool -> a -> a
putBit i b v = if b then setBit v i else clearBit v i

data State = State { mask :: Word64 -> Word64, mem :: IntMap Word64 }

initState :: State
initState = State { mask = id, mem = IntMap.empty }

step :: State -> Inst -> State
step s (Mask mmbs) = s { mask = mask' }
  where
    mibs = catMaybes $ zipWith (fmap . (,)) [0..] mmbs
    mfs = map (uncurry putBit) mibs
    mask' = foldr1 (.) mfs
step s (Mem a d) = s { mem = mem' }
  where
    d' = mask s $ d
    mem' = IntMap.insert a d' $ mem s

solve :: Case -> Soln
solve = sum . IntMap.elems . mem . foldl step initState
