module Expr (evalExpr) where

import Parser (num, parseInt)
import Pass2 (SymbolTable)

import Text.ParserCombinators.Parsec (parse)

evalExpr :: Monad m => SymbolTable -> String -> m Int
evalExpr symTable str = case parse num "" str of
  Left _  -> case lookup str symTable of
    Nothing  -> fail $ "undefined symbol " ++ str
    Just v   -> return v
  Right _ -> parseInt str
