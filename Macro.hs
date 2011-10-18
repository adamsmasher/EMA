module Macro (expandMacros) where

import Parser (Expr(..), Line(..), symbolString)

import Data.List (delete, elemIndex)

-- a macro is a list of parameters and a set of lines to replace
data Macro = Macro [String] [Line] deriving Eq
type MacroTable = [(String, Macro)]

defaultMacros = []

expandMacros :: Monad m => [Line] -> m [Line]
expandMacros src = do (src, macros) <- getMacros src
                      expandMacros' (macros ++ defaultMacros) src

expandMacros' :: Monad m => MacroTable -> [Line] -> m [Line]
expandMacros' ms ls = do
  expanded <- mapM (expandLine ms) ls
  return $ concat expanded

getMacros :: Monad m => [Line] -> m ([Line], MacroTable)
getMacros [] = return ([], [])
getMacros ((CmdLine ".macro" params):rest) = do
  (name:params) <- mapM symbolString params 
  (mLines, after) <- getRestOfMacro rest
  (rest, restMacros) <- getMacros after
  let m = Macro params mLines
  return (rest, ((name, m):restMacros))
getMacros (l:ls) = do
  (restLines, ms) <- getMacros ls
  return $ (l:restLines, ms)

getRestOfMacro :: Monad m => [Line] -> m ([Line], [Line])
getRestOfMacro [] = fail "Unterminated macro"
getRestOfMacro ((CmdLine ".endmacro" args):rest) = case args of
  [] -> return ([], rest)
  _  -> fail "unexpected arguments to endmacro"
getRestOfMacro (l:ls) = do (mLines, after) <- getRestOfMacro ls
                           return (l:mLines, after)

expandLine ms l@(CmdLine cmd args) = case lookup cmd ms of
  Nothing -> return [l]
  Just m  -> expandMacros' (delete (cmd, m) ms) (expandMacro m args)
expandLine ms l = return [l]

expandMacro :: Macro -> [Expr] -> [Line]
expandMacro (Macro params lines) args = map (substituteArgs params args) lines

substituteArgs :: [String] -> [Expr] -> Line -> Line
substituteArgs params subs (CmdLine cmd args) = 
    CmdLine cmd (map (substituteArg params subs) args)

substituteArg :: [String] -> [Expr] -> Expr -> Expr
substituteArg params subs expr = case expr of
  Symbol s -> case s `elemIndex` params of
    Nothing -> expr
    Just i  -> subs !! i
  OffsetBase e r -> OffsetBase (sub e) r
  Negate e  -> Negate (sub e)
  Comp e    -> Comp (sub e)
  BinOp e1 e2 op -> BinOp (sub e1) (sub e2) op
  _ -> expr
 where sub = substituteArg params subs
