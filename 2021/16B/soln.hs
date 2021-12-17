import           Data.Char                    (digitToInt, isSpace)
import           Data.Maybe                   (listToMaybe)
import           Text.ParserCombinators.ReadP (ReadP, (+++), (<++))
import qualified Text.ParserCombinators.ReadP as ReadP
import           Text.Printf                  (printf)

type Case = String
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = showSolns . map solve . readCases

readCases :: String -> [Case]
readCases = lines

showSolns :: [Soln] -> String
showSolns = unlines . map show

hexStringToBinString :: String -> String
hexStringToBinString [] = ""
hexStringToBinString (h:hs) = (printf "%04b" $ digitToInt h) ++ (hexStringToBinString hs)

type Version = Int
type TypeId = Int
data Packet = Literal Version Int | Operation Version TypeId [Packet] deriving (Show)

boolsToInt :: [Bool] -> Int
boolsToInt = sum . zipWith (*) (iterate (2 *) 1) . map fromEnum . reverse

parseBit :: ReadP Bool
parseBit = (== '1') <$> ReadP.satisfy (`elem` "01")

parseBits :: Int -> ReadP [Bool]
parseBits c = ReadP.count c parseBit

parseVersion :: ReadP Int
parseVersion = boolsToInt <$> parseBits 3

parseLiteralBits :: ReadP [Bool]
parseLiteralBits = do
  bss <- ReadP.manyTill (ReadP.char '1' >> parseBits 4) (ReadP.char '0')
  lbs <- parseBits 4
  return $ concat $ bss ++ [lbs]

parseLiteral :: ReadP Packet
parseLiteral = do
  ver <- parseVersion
  ReadP.string "100"
  val <- boolsToInt <$> parseLiteralBits
  return $ Literal ver val

parseTypeID :: ReadP Int
parseTypeID = boolsToInt <$> parseBits 3

parsePacketsWithLen :: ReadP [Packet]
parsePacketsWithLen = do
  ReadP.char '0'
  l <- boolsToInt <$> parseBits 15
  bs <- ReadP.count l ReadP.get
  let ps = fmap fst $ listToMaybe $ ReadP.readP_to_S (ReadP.manyTill parsePacket ReadP.eof) bs
  maybe ReadP.pfail return ps

parsePacketsWithNum :: ReadP [Packet]
parsePacketsWithNum = do
  ReadP.char '1'
  n <- boolsToInt <$> parseBits 11
  ReadP.count n parsePacket

parseOperation :: ReadP Packet
parseOperation = do
  ver <- parseVersion
  tid <- parseTypeID
  os <- parsePacketsWithLen +++ parsePacketsWithNum
  return $ Operation ver tid os

parsePacket :: ReadP Packet
parsePacket = parseLiteral <++ parseOperation

decodeOperation :: Int -> ([Int] -> Int)
decodeOperation 0 = sum
decodeOperation 1 = product
decodeOperation 2 = minimum
decodeOperation 3 = maximum
decodeOperation 4 = error "That's a literal"
decodeOperation 5 = (\(l:r:_) -> if l > r then 1 else 0)
decodeOperation 6 = (\(l:r:_) -> if l < r then 1 else 0)
decodeOperation 7 = (\(l:r:_) -> if l == r then 1 else 0)

evaluatePacket :: Packet -> Int
evaluatePacket (Literal _ val) = val
evaluatePacket (Operation _ tid os) = decodeOperation tid $ map evaluatePacket os

solve :: Case -> Soln
solve = evaluatePacket . fst . head . ReadP.readP_to_S parsePacket . hexStringToBinString
