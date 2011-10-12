module Util where

import Control.Monad
import Data.Bits ((.&.), shiftR)
import Data.Char (intToDigit, isSpace, toLower)
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

readBin = readInt 2 (\c -> c == '0' || c == '1')
                    (\c -> case c of
                      '0' -> 0
                      '1' -> 1) 

showHex n = showIntAtBase 16 intToDigit n "0x"

trimLeft :: String -> String
trimLeft = dropWhile isSpace

w8 :: MonadPlus m => Int -> m Word8
w8 n = do guard (n < 256 && n >= -128)
          return $ fromIntegral n

w16 :: MonadPlus m => Int -> m [Word8]
w16 n = do
  high <- w8 $ n `shiftR` 8
  low  <- w8 $ n .&. 0x00FF 
  guard (n < 65536 && n >= -32768)
  return [high, low]

w32 :: MonadPlus m => Int -> m [Word8]
w32 n = do
  high <- w8 $ n `shiftR` 24
  m1   <- w8 $ (n .&. 0x00FF0000) `shiftR` 16
  m2   <- w8 $ (n .&. 0x0000FF00) `shiftR` 8
  low  <- w8 $ n .&. 0x000000FF
  guard (n < 0xFFFFFFFF && n >= -(2^32))
  return [high, m1, m2, low]

