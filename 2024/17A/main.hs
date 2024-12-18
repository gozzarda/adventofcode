module Main where

import Data.Array.Unboxed
import Data.Bits
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Maybe
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

type Regs = (Word, Word, Word)

type Prob = (Regs, [Word])

type Soln = [Word]

main :: IO ()
main = interact process

process :: String -> String
process = showSoln . solve . readProb

showSoln :: Soln -> String
showSoln = unlines . return . intercalate "," . map show

readProb :: String -> Prob
readProb = doReadP' $ parseProb <* eof

doReadP' :: ReadP a -> String -> a
doReadP' p = fromMaybe (error "Parsing failed") . doReadP p

doReadP :: ReadP a -> String -> Maybe a
doReadP p = fmap fst . listToMaybe . readP_to_S p

parse :: (Read a) => ReadP a
parse = ReadPrec.readPrec_to_P readPrec ReadPrec.minPrec

parseReg :: String -> ReadP Word
parseReg name = do
  string "Register "
  string name
  char ':'
  skipSpaces
  parse

parseRegs :: ReadP Regs
parseRegs = do
  a <- parseReg "A"
  skipSpaces
  b <- parseReg "B"
  skipSpaces
  c <- parseReg "C"
  return (a, b, c)

parseROM :: ReadP [Word]
parseROM = do
  string "Program:"
  skipSpaces
  sepBy parse (char ',')

parseProb :: ReadP Prob
parseProb = do
  regs <- parseRegs
  skipSpaces
  prog <- parseROM
  return (regs, prog)

solve :: Prob -> Soln
solve (regs, prog) = statState $ fromJust haltState
  where
    rom = listArray (0, fromIntegral $ length prog - 1) prog
    states = iterate (stepState rom) (initState regs)
    haltState = listToMaybe $ dropWhile isRunning states

type Addr = Word

type ROM = UArray Addr Word

data Mode = Error String | Halt | Run deriving (Ord, Eq, Show)

-- (mode, pc, regs, out)
type State = (Mode, Addr, Regs, Seq Word)

initState :: Regs -> State
initState regs = (Run, 0, regs, Seq.empty)

stepState :: ROM -> State -> State
stepState _ st@(mode, _, _, _) | mode /= Run = st
stepState rom st@(_, pc, _, _) =
  case (,) <$> (rom !? pc) <*> (rom !? succ pc) of
    Nothing -> opHALT st
    Just (opc, arg) -> case ops !? opc of
      Nothing -> opERR st $ "Unknown opcode " ++ show opc
      Just op -> op rom st arg

isRunning :: State -> Bool
isRunning (Run, _, _, _) = True
isRunning _ = False

statState :: State -> [Word]
statState (_, _, _, out) = toList out

ops :: Array Word (ROM -> State -> Word -> State)
ops = listArray (0, 7) [opADV, opBXL, opBST, opJNZ, opBXC, opOUT, opBDV, opCDV]

combo :: Regs -> Word -> Word
combo _ v | v <= 3 = v
combo (ra, _, _) 4 = ra
combo (_, rb, _) 5 = rb
combo (_, _, rc) 6 = rc

opHALT :: State -> State
opHALT (_, pc, regs, out) = (Halt, pc, regs, out)

opERR :: State -> String -> State
opERR (_, pc, regs, out) msg = (Error msg, pc, regs, out)

opADV :: ROM -> State -> Word -> State
opADV _ (mode, pc, regs@(ra, rb, rc), out) arg =
  (mode, pc + 2, (shiftR ra $ fromIntegral (combo regs arg), rb, rc), out)

opBXL :: ROM -> State -> Word -> State
opBXL _ (mode, pc, regs@(ra, rb, rc), out) arg =
  (mode, pc + 2, (ra, rb `xor` arg, rc), out)

opBST :: ROM -> State -> Word -> State
opBST _ (mode, pc, regs@(ra, rb, rc), out) arg =
  (mode, pc + 2, (ra, combo regs arg .&. 7, rc), out)

opJNZ :: ROM -> State -> Word -> State
opJNZ _ (mode, pc, regs@(ra, rb, rc), out) arg =
  if ra == 0
    then (mode, pc + 2, regs, out)
    else (mode, arg, regs, out)

opBXC :: ROM -> State -> Word -> State
opBXC _ (mode, pc, regs@(ra, rb, rc), out) _ =
  (mode, pc + 2, (ra, rb `xor` rc, rc), out)

opOUT :: ROM -> State -> Word -> State
opOUT _ (mode, pc, regs@(ra, rb, rc), out) arg =
  (mode, pc + 2, regs, out |> (combo regs arg .&. 7))

opBDV :: ROM -> State -> Word -> State
opBDV _ (mode, pc, regs@(ra, rb, rc), out) arg =
  (mode, pc + 2, (ra, shiftR ra $ fromIntegral (combo regs arg), rc), out)

opCDV :: ROM -> State -> Word -> State
opCDV _ (mode, pc, regs@(ra, rb, rc), out) arg =
  (mode, pc + 2, (ra, rb, shiftR ra $ fromIntegral (combo regs arg)), out)
