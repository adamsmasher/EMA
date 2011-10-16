module Util where

import Control.Monad
import Data.Bits ((.&.), shiftR)
import Data.Char (digitToInt, intToDigit, isSpace, toLower)
import Data.Word (Word8)
import Numeric (readDec, readHex, readInt, showIntAtBase)

readNum :: Monad m => String -> m Int
readNum = readNum' . (map toLower)
readNum' ('0':'x':rest) = readNum'' readHex rest
readNum' ('%':rest)     = readNum'' readBin rest
readNum' str            = readNum'' readDec str
readNum'' reader str = case reader str of
  [] -> fail "readNum on empty string"
  ((n,""):[])  -> return n
  ((n,bad):[]) -> fail $ "unexpected " ++ bad

readBin = readInt 2 (\c -> c == '0' || c == '1') digitToInt

showHex n = showIntAtBase 16 intToDigit n "0x"

trimLeft :: String -> String
trimLeft = dropWhile isSpace

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

