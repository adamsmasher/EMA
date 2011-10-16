module Assembler (Bytecode, assemble, makeI, makeR, makeJ) where

import Util (showHex, w32)
import Register (RegInt)

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Maybe (fromJust)
import Data.Word (Word8)

data Bytecode = 
    RInstruction Opcode RegInt RegInt RegInt ShiftAmount FuncCode
  | IInstruction Opcode RegInt RegInt Int
  | JInstruction Opcode Int
  deriving Show

makeI _ _ _ imm | imm > 0xFFFF || imm < -(2^15) =
  fail $ "constant " ++ (show imm) ++ " is too big (must be 16-bit)"
makeI op r1 r2 imm = return $ IInstruction op r1 r2 imm
makeR _ _ _ _ sa _ | sa < 0 || sa >= (2^5) =
  fail $ "shift amount " ++ (show sa) ++ " is too big (must be 5-bit)"
makeR op r1 r2 r3 sa f = return $ RInstruction op r1 r2 r3 sa f
makeJ _ t | t >= (2^28) =
  fail $ "jump target " ++ (showHex t) ++ " out of range"
makeJ _ t | t .&. 3 /= 0 =
  fail $ "jump target " ++ (showHex t) ++ " not word aligned!"
makeJ op t = return $ JInstruction op (t `shiftR` 2) 

type ShiftAmount = Int

type Opcode = Int

type FuncCode = Int

assemble :: Bytecode -> [Word8]
assemble (RInstruction op rs rt rd shamt funct) =
  fromJust $ w32 ( op `shiftL` 26
    .|. rs `shiftL` 21
    .|. rt `shiftL` 16
    .|. rd `shiftL` 11
    .|. shamt `shiftL`  6
    .|. funct)
assemble (IInstruction op rs rt immediate) =
  fromJust $ w32 ( op `shiftL` 26
    .|. rs `shiftL` 21
    .|. rt `shiftL` 16
    .|. (immediate .&. 0xFFFF))
assemble (JInstruction op addr) =
  fromJust $ w32 ( op `shiftL` 26
    .|. addr)

