module Util where

import Data.Bits ((.&.), shiftR)
import Data.Char (digitToInt, intToDigit)
import Data.Word (Word8)
import Numeric (readInt, showIntAtBase)

readBin :: String -> [(Int, String)]
readBin = readInt 2 (\c -> c == '0' || c == '1') digitToInt

showHex n = showIntAtBase 16 intToDigit n "0x"

w8 :: Monad m => Int -> m Word8
w8 n | n >= 2^8 || n < -(2^7) =
  fail $ "byte constant " ++ (show n) ++ " cannot fit in a byte"
w8 n = return $ fromIntegral n

w16 :: Monad m => Int -> m [Word8]
w16 n | n >= 2^16 || n < -(2^15) =
  fail $ "halfword constant " ++ (show n) ++ " cannot fit in a halfword"
w16 n = do
  high <- w8 $ n `shiftR` 8
  low  <- w8 $ n .&. 0x00FF 
  return [high, low]

w32 :: Monad m => Int -> m [Word8]
w32 n = do
  high <- w8 $ n `shiftR` 24
  m1   <- w8 $ (n .&. 0x00FF0000) `shiftR` 16
  m2   <- w8 $ (n .&. 0x0000FF00) `shiftR` 8
  low  <- w8 $ n .&. 0x000000FF
  return [high, m1, m2, low]

doTimes :: Monad m => Int -> (Int -> m b) -> m ()
doTimes 0 _ = return ()
doTimes n f = f n >> doTimes (n-1) f

invalidArgs s = fail $ "invalid arguments given for command " ++ s
