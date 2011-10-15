module Register where

import Data.Char (isDigit, toLower)
import qualified Debug.Trace as Debug

type RegInt = Int

lookupRegisterName :: (Monad m) => String -> m Int
lookupRegisterName str
  | all isDigit str = case read (Debug.trace str str) of
      n | n > 31 -> fail "Invalid register number"
      n          -> return n
  | otherwise = case map toLower str of
    "zero" -> return 0
    "at"   -> return 1
    "v0"   -> return 2
    "v1"   -> return 3
    "a0"   -> return 4
    "a1"   -> return 5
    "a2"   -> return 6
    "a3"   -> return 7
    "t0"   -> return 8
    "t1"   -> return 9
    "t2"   -> return 10
    "t3"   -> return 11
    "t4"   -> return 12
    "t5"   -> return 13
    "t6"   -> return 14
    "t7"   -> return 15
    "s0"   -> return 16
    "s1"   -> return 17
    "s2"   -> return 18
    "s3"   -> return 19
    "s4"   -> return 20
    "s5"   -> return 21
    "s6"   -> return 22
    "s7"   -> return 23
    "t8"   -> return 24
    "t9"   -> return 25
    "k0"   -> return 26
    "k1"   -> return 27
    "gp"   -> return 28
    "sp"   -> return 29
    "fp"   -> return 30
    "ra"   -> return 31
    _      -> fail $ str ++ " is not a valid register"
-- lookupRegisterName should never get "" strings, if the parse works right
lookupRegisterName _ = error "Internal assembler error - lookupRegisterName"

