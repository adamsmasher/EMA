module Parser where

import Register (lookupRegisterName)
import Util (readBin)

import Control.Monad (liftM)
import Data.Maybe (fromJust, maybeToList)
import Numeric (readHex)
import Text.Parsec.Error (Message(SysUnExpect, UnExpect, Expect, Message), 
                          errorPos, errorMessages)
import Text.ParserCombinators.Parsec (Parser, ParseError, sourceColumn)
import Text.ParserCombinators.Parsec.Char (alphaNum, anyChar, char, digit,
                                           hexDigit, letter, oneOf, satisfy, 
                                           spaces, string)
import Text.ParserCombinators.Parsec.Combinator (between, eof, many1, option,
                                                 optional, sepBy)
import Text.ParserCombinators.Parsec.Prim ((<|>), many, parse, try)

data Line = Label String | CmdLine String [Expr] deriving Show
data Expr = Str String
          | Symbol String
          | Register Int
          | Hex Int
          | Bin Int
          | Dec Int
          | OffsetBase Expr Int
  deriving Show

regNum (Register n) = Just n
regNum _ = Nothing

symbolString (Symbol s) = Just s
symbolString _ = Nothing

asmline = do spaces
             l <- try (option Nothing (label >>= return . Just))
             spaces
             cmd <- try (option Nothing (cmdline >>= return . Just))
             spaces
             eof
             return $ (maybeToList l) ++ (maybeToList cmd)
             
label :: Parser Line
label = do first <- letter <|> char '_'
           restLabel <- many (alphaNum <|> char '_')
           char ':'
           return $ Label (first:restLabel)

cmdline :: Parser Line
cmdline = do cmd <- symbol
             spaces
             args <- argList
             return $ CmdLine (fromJust $ symbolString cmd) args

argList :: Parser [Expr]
argList = arg `sepBy` (char ',' >> spaces)

arg = do a <- try offsetBase
                <|> num
                <|> (try symbol) 
                <|> register 
         spaces
         return a

offsetBase :: Parser Expr
offsetBase = do n <- option (Dec 0) num
                char '('
                spaces
                r <- register
                spaces
                char ')'
                return $ OffsetBase n (fromJust (regNum r))

num :: Parser Expr
num = (try hex) <|> bin <|> dec
hex = do char '0'
         char 'X' <|> char 'x'
         ds <- many1 hexDigit
         return $ Hex (fst $ head (readHex ds))

dec = do ds <- many1 digit
         return $ Dec (read ds)

bin = do char '%'
         ds <- many1 (oneOf "01")
         return $ Bin (fst $ head (readBin ds))

symbol :: Parser Expr
symbol = do first <- letter <|> char '_' <|> char '.'
            rest <- many (alphaNum <|> char '_')
            return $ Symbol (first:rest)

-- taken from Text.Parsec.Token, with modifications
stringLiteral :: Parser Expr
stringLiteral = do s <- between (char '"') (char '"') (many stringChar)
                   return $ Str s

stringChar = stringLetter <|> stringEscape

stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape = char '\\' >> char '"'
         
register = char '$' >> many1 alphaNum >>= lookupRegisterName >>=
           return . Register

-- parses is a utility that tests only if a String can be parsed by a parser
parses p str = case parse p "" str of
  Left _  -> False
  Right _ -> True

prettyParseError :: String -> ParseError -> String
prettyParseError msg err =
  let pos  = errorPos err
      msgs = filter (not . null) $ map errorMsgString (errorMessages err) in
  (unlines msgs) ++ msg

errorMsgString :: Message -> String
errorMsgString (SysUnExpect "") = ""
errorMsgString (SysUnExpect s)  = "unexpected " ++ s
errorMsgString (UnExpect "")    = ""
errorMsgString (UnExpect s)     = "unexpected " ++ s
errorMsgString (Expect "")      = ""
errorMsgString (Expect s)       = "expected " ++ s
errorMsgString (Message s)      = s

