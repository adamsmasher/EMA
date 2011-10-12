module Parser where

import Register (RegInt(..), lookupRegisterName)
import Text.Parsec.Error (Message(SysUnExpect, UnExpect, Expect, Message), 
                          errorPos, errorMessages)
import Text.ParserCombinators.Parsec (Parser, ParseError, sourceColumn)
import Text.ParserCombinators.Parsec.Char (alphaNum, anyChar, char, digit,
                                           hexDigit, letter, oneOf, satisfy, 
                                           spaces, string)
import Text.ParserCombinators.Parsec.Combinator (between, eof, many1, option,
                                                 optional, sepBy)
import Text.ParserCombinators.Parsec.Prim ((<|>), many, parse, try)
import Util (readNum)

data ConstType = Byte [String] | Half [String] | Word [String] deriving Show
data Include = Incsrc String | Incbin String deriving Show
data Line = Line String [String] deriving Show

include = (try incbin) <|> incsrc

incsrc = string ".include " >> spaces >> stringLiteral >>= return . Incsrc
incbin = string ".incbin " >> spaces >> stringLiteral >>= return . Incbin

-- taken from Text.Parsec.Token, with modifications
stringLiteral :: Parser String
stringLiteral = between (char '"')
                        (char '"')
                        (many stringChar)

stringChar = stringLetter <|> stringEscape

stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape = char '\\' >> char '"'

label :: Parser (String, String)
label = do first <- letter <|> char '_'
           restLabel <- many (alphaNum <|> char '_')
           char ':'
           spaces
           restLine <- many anyChar
           return $ (first:restLabel, restLine)

constDirective = do char '.'
                    size <- string "byte" <|> string "half" <|> string "word"
                    spaces
                    args <- argList
                    spaces
                    eof
                    return $ constOf size args

constOf "byte" = Byte
constOf "half" = Half
constOf "word" = Word


parseSectionHeader :: Monad m => String -> m Int
parseSectionHeader str = case parse sectionHeader "" str of
  Left _     -> fail "Error parsing section header"
  Right nStr -> readNum nStr

sectionHeader = string ".text" >> spaces >> num

argList = arg `sepBy` (char ',' >> spaces)

arg = do a <- try (offsetBase >>=
                     (\(n, base) -> return $ n ++ "(" ++ base ++ ")"))
                <|> num
                <|> (try symbol) 
                <|> register 
         spaces
         return a
          
symbol = do first <- letter <|> char '_'
            rest <- many (alphaNum <|> char '_')
            return $ first:rest

register = do {char '$'; r <- many1 alphaNum; return $ '$':r;}

parseRegister :: Monad m => String -> m RegInt
parseRegister str = case parse register "" str of
  Left err    -> fail "Could not parse register"
  Right rName -> lookupRegisterName rName

-- parses is a utility that tests only if a String can be parsed by a parser
parses p str = case parse p "" str of
  Left _  -> False
  Right _ -> True

parseLine :: Monad m => String -> m Line
parseLine str = case parse parser "" str of
  Left err          -> fail $ prettyParseError "Could not parse line" err
  Right (i, params) -> return $ Line i params
  where parser = do spaces
                    i <- symbol
                    spaces
                    args <- argList
                    spaces
                    eof
                    return $ (i, args)

parseOffsetBase :: Monad m => String -> m (Int, RegInt)
parseOffsetBase str = case parse offsetBase "" str of
  Left err           -> fail $ "Error parsing offset"
  Right (nStr, rStr) -> do r <- parseRegister rStr
                           n <- readNum nStr
                           return (n, r)

offsetBase = do n <- option "0" num
                char '('
                spaces
                r <- many1 (alphaNum <|> char '$')
                spaces
                char ')'
                return $ (n, r)

num = try (string "0x" >> many1 hexDigit >>= (\n -> return $ "0x" ++ n)) 
  <|> (many1 digit)
  <|> (char '%' >> many1 (oneOf "01"))

parseInt :: Monad m => String -> m Int
parseInt str = case parses num str of
  False -> fail $ "Error parsing number '" ++ str ++ "'"
  True  -> readNum str

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


