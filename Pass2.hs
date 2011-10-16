module Pass2 (SymbolTable, buildSymbolTable) where

import Control.Monad.State (State(..), get, put, evalState)
import Parser (Line(..), Expr(..))

type SymbolTable = [(String, Int)]

-- our sym table build is a process that stores its state in a BuildST
-- and its result is a pair of the remaining lines (strings + addresses) and a
-- symbol table
type SymTableBuild = State BuildST ([(Int, Line)], SymbolTable)
data BuildST = BuildST { bstLines_acc :: [(Int, Line)],
                         bstSymbols   :: SymbolTable,
                         bstAddr      :: Maybe Int }
startState = BuildST [] [] Nothing

newSection s = get >>= (\st -> put st {bstAddr = (Just s)})

moveAhead n = get >>= (\st -> case bstAddr st of
  Nothing   -> fail "FUCK A FUCKING DUCK"
  Just addr -> put st {bstAddr = Just $ addr + n})

-- a is alignment (0 is no align, 1 is half, 2 is word...)
addLine l a = get >>= (\st -> case bstAddr st of
  Nothing   -> fail "SUCK MY FUCKING CoCK"
  Just addr -> do
    let align = 2^a - addr `mod` 2^a
    moveAhead align
    put st {bstLines_acc = ((addr + align, l):(bstLines_acc st))}
    moveAhead (byteSize l))

addSymbol s = get >>= (\st -> case bstAddr st of
  Nothing   -> fail "FUCK YOU ASSHOLE"
  Just addr -> case lookup s (bstSymbols st) of
    Nothing -> put st { bstSymbols = (s, addr):(bstSymbols st) }
    Just _  -> fail $ "Duplicate symbol " ++ s)

returnCurrentResults = do st <- get
                          return $ (reverse $ bstLines_acc st, bstSymbols st)

buildSymbolTable :: [Line] -> ([(Int, Line)], SymbolTable)
buildSymbolTable [] = ([], []) -- chain requires non-empty lists
buildSymbolTable src =
  evalState (chain $ map doLine src)  startState 

chain = foldr1 (>>)
  
doLine :: Line -> SymTableBuild
doLine (Label s) = do addSymbol s
                      returnCurrentResults
doLine (CmdLine ".text" args) = case args of
  ((Num _ n):[]) -> do newSection n
                       returnCurrentResults
  _              -> fail "Huge fucking screwup"
doLine (CmdLine ".align" args) = case args of
  ((Num _ n):[]) -> get >>= (\st ->
    case bstAddr st of
      Nothing -> fail "EAT baLLS"
      Just addr -> do
        let align = 2^n - addr `mod` 2^n
        moveAhead align
        returnCurrentResults)
  _              -> fail "Eat a dick."
doLine l@(CmdLine ".ascii" args) = case args of
  ((Str s):[]) -> do addLine l 0
                     returnCurrentResults
  _            -> fail "Ballsacks!"
doLine l@(CmdLine ".asciiz" args) = case args of
  ((Str s):[]) -> do addLine l 0
                     returnCurrentResults
  _            -> fail "Ballsackz!"
doLine l@(CmdLine ".byte" _) = do addLine l 0
                                  returnCurrentResults
doLine l@(CmdLine ".half" _) = do addLine l 1
                                  returnCurrentResults
doLine l  = do addLine l 2
               returnCurrentResults

byteSize :: Line -> Int
byteSize (Label _) = 0
byteSize (CmdLine ".byte" args) = length args
byteSize (CmdLine ".half" args) = length args * 2
byteSize (CmdLine ".word" args) = length args * 4
byteSize (CmdLine _ _)          = 4
