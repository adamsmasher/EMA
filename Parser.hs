module Parser (Line(..), LineType(..), Expr(..), NumType(..), getOp, parseFile, 
               regNum, symbolString) where

import Register (lookupRegisterName)
import Util (readBin)

import Control.Arrow ((>>>))
import Control.Applicative ((<$>), (<*>))
import Data.Bits ((.|.), (.&.), shiftL, shiftR, xor)
import Data.Char(isSpace)
import Data.Maybe (fromJust, maybeToList)
import Numeric (readHex)
import Text.Parsec.Error (Message(SysUnExpect, UnExpect, Expect, Message), 
                          errorPos, errorMessages)
import Text.ParserCombinators.Parsec (Parser, ParseError, sourceLine)
import Text.ParserCombinators.Parsec.Char (alphaNum, char, digit, hexDigit,
                                           letter, newline, oneOf, satisfy,
                                           string)
import Text.ParserCombinators.Parsec.Combinator (between, many1, option,
                                                 optionMaybe, sepBy)
import Text.ParserCombinators.Parsec.Prim ((<|>), getPosition, many, parse, try)

------ types -------------------------------------------------

data Line = Line LineType Int deriving (Show, Eq)
data LineType = Label String | CmdLine String [Expr] deriving (Show, Eq)
data Expr = Str String
          | Symbol String
          | Register Integer
          | Num NumType Integer
          | OffsetBase Expr Integer
          | Negate Expr
          | Comp Expr
          | BinOp Expr Expr BinOpType
  deriving (Show, Eq)
data NumType = Hex | Dec | Bin deriving (Show, Eq)
data BinOpType = Add | Sub | Mul | Div | And | Or | Xor | Lsh | Rsh
  deriving (Show, Eq)

------ public -------------------------------------------------

parseFile f str = parse asmsrc f (removeComments str) >>= return . concat

------ utility -------------------------------------------------

removeComments :: String -> String
removeComments = lines >>> map stripComment >>> unlines

-- comments begin with a #, carry on to end of line
stripComment :: String -> String
stripComment = takeWhile (/='#')

regNum (Register n) = Just n
regNum _ = Nothing

symbolString (Symbol s) = return s
symbolString _ = fail $ "not a symbol"

getOp :: BinOpType -> (Integer -> Integer -> Integer)
getOp Add = (+)
getOp Sub = (-)
getOp Mul = (*)
getOp Div = div
getOp Lsh = \x y -> x `shiftL` (fromIntegral y)
getOp Rsh = \x y -> x `shiftR` (fromIntegral y)
getOp And = (.&.)
getOp Or  = (.|.)
getOp Xor = xor

------ parsers -------------------------------------------------

asmsrc = many asmline

asmline = do whitespaces
             l <- optionMaybe (try label)
             whitespaces
             cmd <- optionMaybe (try cmdline)
             whitespaces
             newline
             return $ (maybeToList l) ++ (maybeToList cmd)
             
whitespace = satisfy (\c -> isSpace c && c /= '\n')
whitespaces = many whitespace

label :: Parser Line
label = do first <- letter <|> char '_'
           restLabel <- many (alphaNum <|> char '_')
           char ':'
           lineNum <- sourceLine <$> getPosition
           return $ Line (Label (first:restLabel)) lineNum 

cmdline :: Parser Line
cmdline = do cmd <- symbol
             whitespaces
             args <- argList
             lineNum <- sourceLine <$> getPosition
             return $ Line (CmdLine (fromJust $ symbolString cmd) args) lineNum

argList :: Parser [Expr]
argList = arg `sepBy` (char ',' >> whitespaces)

arg = do a <- try offsetBase
                <|> register
                <|> stringLiteral
                <|> expr
         whitespaces
         return a

expr = l7

l0 = paren
atom = num <|> symbol <|> l0
l1 = negcomp <|> atom
l2 = try muldivExpr <|> l1
l3 = try addsubExpr <|> l2
l4 = try shiftExpr <|> l3
l5 = try andExpr <|> l4
l6 = try xorExpr <|> l5
l7 = try orExpr <|> l6

paren = between (char '(' >> whitespaces) (whitespaces >> char ')') expr

negcomp = do op <- char '-' <|> char '~'
             e <- atom
             case op of
               '-' -> return $ Negate e
               '~' -> return $ Comp e

muldivExpr = do e1 <- l1
                whitespaces
                op <- char '*' <|> char '/'
                whitespaces
                e2 <- l2
                case op of
                  '*' -> return $ BinOp e1 e2 Mul
                  '/' -> return $ BinOp e1 e2 Div

addsubExpr = do e1 <- l2
                whitespaces
                op <- char '+' <|> char '-'
                whitespaces
                e2 <- l3
                case op of
                  '+' -> return $ BinOp e1 e2 Add
                  '-' -> return $ BinOp e1 e2 Sub

shiftExpr = do e1 <- l3
               whitespaces
               op <- string "<<" <|> string ">>"
               whitespaces
               e2 <- l4
               case op of
                 "<<" -> return $ BinOp e1 e2 Lsh
                 ">>" -> return $ BinOp e1 e2 Rsh

andExpr = do e1 <- l4
             whitespaces
             char '&'
             whitespaces
             e2 <- l5
             return $ BinOp e1 e2 And

xorExpr = do e1 <- l5
             whitespaces
             char '^'
             whitespaces
             e2 <- l6
             return $ BinOp e1 e2 Xor

orExpr = do e1 <- l6
            whitespaces
            char '|'
            whitespaces
            e2 <- l7
            return $ BinOp e1 e2 Or

offsetBase :: Parser Expr
offsetBase = do n <- option (Num Dec 0) (try expr)
                char '('
                whitespaces
                r <- register
                whitespaces
                char ')'
                return $ OffsetBase n (fromJust (regNum r))

num :: Parser Expr
num = (try hex) <|> bin <|> dec
hex = do char '0'
         char 'X' <|> char 'x'
         ds <- many1 hexDigit
         return $ Num Hex (fst $ head (readHex ds))

dec = do ds <- many1 digit
         return $ Num Dec (read ds)

bin = do char '%'
         ds <- many1 (oneOf "01")
         return $ Num Bin (fst $ head (readBin ds))

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

