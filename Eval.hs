module Eval (evalExpr) where

import Parser (Expr(..))
import Pass2 (SymbolTable)

import Text.ParserCombinators.Parsec (parse)

evalExpr :: Monad m => SymbolTable -> Expr -> m Int
evalExpr symTable e = case e of
  Str s    -> fail $ "unexpected string literal"
  Symbol s -> case lookup s symTable of
    Nothing -> fail $ "undefined symbol " ++ s
    Just v  -> return v
  Register _ -> fail "unexpected register"
  Hex n -> return n
  Bin n -> return n
  Dec n -> return n
  OffsetBase _ _ -> fail "unexpected offset base pair"
   
