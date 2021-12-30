import           Data.List.Split (splitOn)
import           Data.Maybe      (catMaybes)
import           Text.Read       (readMaybe)

type Case = (Integer, [Maybe Integer])
type Soln = Integer

main :: IO ()
main = interact handle

handle :: String -> String
handle = display . solve . parse

parse :: String -> Case
parse s = (t, ms)
  where
    (ts:mss:_) = lines s
    t = read ts
    ms = map readMaybe $ splitOn "," mss

display :: Soln -> String
display = unlines . return . show

crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
  where
    go (rl, ml) (rr, mr) = (mod r m, m)
      where
        r = rr + mr * (rl - rr) * (inv mr ml)
        m = mr * ml
    inv a m = s `mod` m
      where
        (_, s, _) = gcd' a m
    gcd' 0 b = (b, 0, 1)
    gcd' a b = (g, t - (div b a) * s, s)
      where
        (g, s, t) = gcd' (mod b a) a

solve :: Case -> Soln
solve (t, ms) = fst $ crt rms
  where
    rms = catMaybes $ zipWith f [0..] ms
    f i = fmap (\b -> (mod (-i) b, b))
