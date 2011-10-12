module Util where

import Data.Bits ((.&.), shiftR)
import Data.Char (isSpace, toLower)
import Data.Word (Word8)
import Numeric (readDec, readHex, readInt)

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

trimLeft :: String -> String
trimLeft = dropWhile isSpace

w8 :: Int -> Word8
w8 = fromIntegral

w16 :: Int -> [Word8]
w16 n =
  let high = n `shiftR` 8
      low  = n .&. 0x00FF in
  [w8 low, w8 high]

w32 :: Int -> [Word8]
w32 n =
  let high = n `shiftR` 24
      m1   = (n .&. 0x00FF0000) `shiftR` 16
      m2   = (n .&. 0x0000FF00) `shiftR` 8
      low  = n .&. 0x000000FF in
  [w8 low, w8 m2, w8 m1, w8 high]

