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

maskFuncs :: (Bits a) => [Maybe Bool] -> [a -> a]
maskFuncs mbs = foldl f [id] imbs
  where
    imbs = zip [0..] mbs
    f fs (_, Just False) = fs
    f fs (i, Just True) = map ((`setBit` i) .) fs
    f fs (i, Nothing) = (map ((`clearBit` i) .) fs) ++ (map ((`setBit` i) .) fs)

maskFunc :: (Bits a) => [Maybe Bool] -> a -> [a]
maskFunc mbs x = map ($ x) (maskFuncs mbs)

data State = State { mask :: Int -> [Int], mem :: IntMap Word64 }

initState :: State
initState = State { mask = const [], mem = IntMap.empty }

step :: State -> Inst -> State
step s (Mask mbs) = s { mask = maskFunc mbs }
step s (Mem a d)  = s { mem = foldr (flip IntMap.insert d) (mem s) (mask s a) }

solve :: Case -> Soln
solve = sum . IntMap.elems . mem . foldl step initState
