module Parser (Line(..), Expr(..), NumType(..), parseFile, regNum, 
               symbolString) where

import Register (lookupRegisterName)
import Util (readBin)

import Control.Arrow ((>>>))
import Data.Char(isSpace)
import Data.Maybe (fromJust, maybeToList)
import Numeric (readHex)
import Text.Parsec.Error (Message(SysUnExpect, UnExpect, Expect, Message), 
                          errorPos, errorMessages)
import Text.ParserCombinators.Parsec (Parser, ParseError, sourceColumn)
import Text.ParserCombinators.Parsec.Char (alphaNum, char, digit, hexDigit,
                                           letter, newline, oneOf, satisfy)
import Text.ParserCombinators.Parsec.Combinator (between, many1, option,
                                                 optionMaybe, sepBy)
import Text.ParserCombinators.Parsec.Prim ((<|>), many, parse, try)

------ types -------------------------------------------------

data Line = Label String | CmdLine String [Expr] deriving (Show, Eq)
data Expr = Str String
          | Symbol String
          | Register Int
          | Num NumType Int
          | OffsetBase Expr Int
  deriving (Show, Eq)
data NumType = Hex | Dec | Bin deriving (Show, Eq)

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
           return $ Label (first:restLabel)

cmdline :: Parser Line
cmdline = do cmd <- symbol
             whitespaces
             args <- argList
             return $ CmdLine (fromJust $ symbolString cmd) args

argList :: Parser [Expr]
argList = arg `sepBy` (char ',' >> whitespaces)

arg = do a <- try offsetBase
                <|> num
                <|> (try symbol) 
                <|> register 
         whitespaces
         return a

offsetBase :: Parser Expr
offsetBase = do n <- option (Num Dec 0) num
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

