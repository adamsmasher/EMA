module Macro (expandMacros) where

import ErrorHandling (AssemblyState(..))
import MIPSConst
import Parser (Expr(..), Line(..), LineType(..), symbolString)

import Data.List (delete, elemIndex)

-- a macro is a list of parameters and a set of lines to replace
data Macro = Macro [String] [LineType] deriving Eq
type MacroTable = [(String, Macro)]

defaultMacros = []

expandMacros :: [Line] -> AssemblyState [Line]
expandMacros src = do (src, macros) <- getMacros src
                      expandMacros' (macros ++ defaultMacros) src

expandMacros' :: MacroTable -> [Line] -> AssemblyState [Line]
expandMacros' ms ls = do
  expanded <- mapM (expandLine ms) ls
  return $ concat expanded

getMacros :: [Line] -> AssemblyState ([Line], MacroTable)
getMacros [] = return ([], [])
getMacros ((Line (CmdLine ".macro" params) lineNum):rest) = do
  (name:params) <- mapM symbolString params 
  checkValidName name lineNum
  (mLines, after) <- getRestOfMacro rest
  (rest, restMacros) <- getMacros after
  let m = Macro params mLines
  return (rest, ((name, m):restMacros))
getMacros (l:ls) = do
  (restLines, ms) <- getMacros ls
  return $ (l:restLines, ms)

checkValidName n lnNum | n `elem` supportedInstructions =
  Error (n ++ " is an instruction name and cannot be the name of a macro")
        (Just lnNum)
checkValidName _ _ = return ()

getRestOfMacro :: [Line] -> AssemblyState ([LineType], [Line])
getRestOfMacro [] = fail "Unterminated macro at EOF" 
getRestOfMacro ((Line (CmdLine ".endmacro" args) lineNum):rest) = case args of
  [] -> return ([], rest)
  _  -> Error "unexpected arguments to endmacro" (Just lineNum)
getRestOfMacro ((Line l _):ls) = do (mLines, after) <- getRestOfMacro ls
                                    return (l:mLines, after)

expandLine :: MacroTable -> Line -> AssemblyState [Line]
expandLine ms l@(Line (CmdLine cmd args) n) = case lookup cmd ms of
  Nothing -> return [l]
  Just m  -> expandMacros' (delete (cmd, m) ms) (expandMacro m args n)
expandLine ms l = return [l]

expandMacro :: Macro -> [Expr] -> Int -> [Line]
expandMacro (Macro params lines) args lineNum =
  map (substituteArgs params args lineNum) lines

substituteArgs :: [String] -> [Expr] -> Int -> LineType -> Line
substituteArgs params subs lineNum (CmdLine cmd args) = 
    Line (CmdLine cmd (map (substituteArg params subs) args)) lineNum

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
