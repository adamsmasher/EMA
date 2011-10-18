module Eval (evalExpr) where

import Parser (Expr(..), getOp)
import Pass2 (SymbolTable)

import Data.Bits (complement)
import Text.ParserCombinators.Parsec (parse)

evalExpr :: Monad m => SymbolTable -> Expr -> m Int
evalExpr symTable e = case e of
  Str s    -> fail $ "unexpected string literal"
  Symbol s -> case lookup s symTable of
    Nothing -> fail $ "undefined symbol " ++ s
    Just v  -> return v
  Register _ -> fail "unexpected register"
  Num _ n -> return n
  OffsetBase _ _ -> fail "unexpected offset base pair"
  Negate e' -> do x <- eval e'
                  return $ -x
  Comp e'   -> do x <- eval e'
                  return $ complement x
  BinOp e1 e2 op -> do e1' <- eval e1
                       e2' <- eval e2
                       return $ (getOp op) e1' e2'
 where eval = evalExpr symTable
   
