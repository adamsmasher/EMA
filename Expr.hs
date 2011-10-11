module Expr (evalExpr) where

import Parser (num)
import SymTable (SymbolTable(..))
import Util (readNum)

import Text.ParserCombinators.Parsec (parse)

evalExpr :: Monad m => SymbolTable -> String -> m Int
evalExpr symTable str = case parse num "" str of
  Left _  -> case lookup str symTable of
    Nothing  -> fail $ "undefined symbol " ++ str
    Just v   -> return v
  Right _ -> case readNum str of
    Left _  -> fail $ "Unable to read number " ++ str
    Right n -> return n
