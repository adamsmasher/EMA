module Assembler where

import Util (w8, w16, w32)
import Register

import Data.Bits ((.&.), (.|.), shiftL)
import Data.List (isPrefixOf)
import Data.Word (Word8)

data Bytecode = 
    RInstruction Opcode RegInt RegInt RegInt ShiftAmount FuncCode
  | IInstruction Opcode RegInt RegInt Int
  | JInstruction Opcode Int
  deriving Show

type ShiftAmount = Int

type Opcode = Int

type FuncCode = Int

byteSize x | ".byte" `isPrefixOf` x = 1
byteSize x = 4

assemble :: Bytecode -> [Word8]
assemble (RInstruction op rs rt rd shamt funct) =
  w32 ( op `shiftL` 26
    .|. rs `shiftL` 21
    .|. rt `shiftL` 16
    .|. rd `shiftL` 11
    .|. shamt `shiftL`  6
    .|. funct)
assemble (IInstruction op rs rt immediate) =
  w32 ( op `shiftL` 26
    .|. rs `shiftL` 21
    .|. rt `shiftL` 16
    .|. (immediate .&. 0xFFFF))
assemble (JInstruction op addr) =
  w32 ( op `shiftL` 26
    .|. addr)

