module Pass2 (SymbolTable, buildSymbolTable) where

import Control.Monad (msum)
import Control.Monad.State (State(..), get, put, evalState)
import Parser (Line(..), Expr(..))
import Text.ParserCombinators.Parsec (parse)

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

addLine s = get >>= (\st -> case bstAddr st of
  Nothing   -> fail "SUCK MY FUCKING CoCK"
  Just addr -> put st {bstLines_acc = ((addr, s):(bstLines_acc st))})

addSymbol s = get >>= (\st -> case bstAddr st of
  Nothing   -> fail "FUCK YOU ASSHOLE"
  Just addr -> put st { bstSymbols = (s, addr):(bstSymbols st) })

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
  ((Hex n):[]) -> do newSection n
                     returnCurrentResults
  ((Dec n):[]) -> do newSection n
                     returnCurrentResults
  ((Bin n):[]) -> do newSection n
                     returnCurrentResults
  _            -> fail "Huge fucking screwup"
doLine l = do addLine l
              moveAhead (byteSize l)
              returnCurrentResults

byteSize :: Line -> Int
byteSize (Label _) = 0
byteSize (CmdLine ".byte" args) = length args
byteSize (CmdLine ".half" args) = length args * 2
byteSize (CmdLine ".word" args) = length args * 4
byteSize (CmdLine _ _)          = 4
