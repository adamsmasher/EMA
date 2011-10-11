module SymTable (SymbolTable, buildSymbolTable) where

import Assembler (byteSize)
import Control.Monad (msum)
import Control.Monad.State (State(..), get, put, evalState)
import Parser (label)
import Text.ParserCombinators.Parsec (parse)

type SymbolTable = [(String, Int)]

-- our sym table build is a process that stores its state in a BuildST
-- and its result is a pair of the lines of the new section and a
-- symbol table
type SymTableBuild = State BuildST ([String], SymbolTable)
data BuildST = BuildST { bstLines_acc :: [String],
                         bstSymbols   :: SymbolTable,
                         bstAddr      :: Int }
startState = BuildST [] []

moveAhead n = get >>= (\st -> put st {bstAddr = (bstAddr st) + n})
addLine s = get >>= (\st -> put st {bstLines_acc = (s:(bstLines_acc st))})
addSymbol s = get >>= (\st -> 
  put st { bstSymbols = (s, bstAddr st):(bstSymbols st) })
returnCurrentResults = do st <- get
                          return $ (reverse $ bstLines_acc st, bstSymbols st)

buildSymbolTable :: Int -> [String] -> ([String], SymbolTable)
buildSymbolTable _ [] = ([], []) -- chain requires non-empty lists
buildSymbolTable start body =
  evalState (chain $ map addLabelFromLine body) (startState start)

chain = foldr1 (>>)
  
addLabelFromLine :: String -> SymTableBuild
addLabelFromLine x = case parse label "" x of
  Left _          -> do addLine x
                        moveAhead (byteSize x)
                        returnCurrentResults
  Right (l, rest) -> do addSymbol l
                        case rest of
                          "" -> returnCurrentResults
                          x  -> addLine x >> returnCurrentResults
