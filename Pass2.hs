module Pass2 (SymbolTable, buildSymbolTable) where

import Parser (Line(..), Expr(..), NumType(..))
import Util (doTimes)

import Control.Monad.State (State(..), get, put, evalState)

type SymbolTable = [(String, Int)]

-- our sym table build is a process that stores its state in a BuildST
-- and its result is a pair of the remaining lines (strings + addresses) and a
-- symbol table
type SymTableBuild = State BuildST ([(Int, Line)], SymbolTable)
data BuildST = BuildST { bstLines_acc   :: [(Int, Line)],
                         bstSymbols     :: SymbolTable,
                         bstSectionType :: Maybe SectionType,
                         bstAddr        :: Maybe Int }
data SectionType = Text | BSS deriving Eq

startState = BuildST [] [] Nothing Nothing

newSection s = get >>= (\st -> put st {bstAddr = (Just s)})
noSection = fail "all code must reside within a .text or .bss section"
invalidSection = fail "line not valid in current section type"
checkSectionType t = get >>= (\st ->
  case (bstSectionType st) == Just t of
    True  -> return ()
    False -> invalidSection)

setSectionType t = get >>= (\st -> put st { bstSectionType = Just t })

moveAhead n = get >>= (\st -> case bstAddr st of
  Nothing   -> noSection
  Just addr -> put st {bstAddr = Just $ addr + n})

-- a is alignment (0 is no align, 1 is half, 2 is word...)
addLine l a = get >>= (\st -> case bstAddr st of
  Nothing   -> noSection
  Just addr -> do
    let align = (2^a - addr `mod` 2^a) `mod` 2^a
    dummyBytes align
    put st {bstLines_acc = ((addr + align, l):(bstLines_acc st))}
    moveAhead (byteSize l))

addSymbol s = get >>= (\st -> case bstAddr st of
  Nothing   -> noSection
  Just addr -> case lookup s (bstSymbols st) of
    Nothing -> put st { bstSymbols = (s, addr):(bstSymbols st) }
    Just _  -> fail $ "Duplicate symbol " ++ s)

dummyBytes n = get >>= (\st -> case bstSectionType st of
    Just Text -> doTimes n (\_ -> addLine (CmdLine ".byte" [Num Hex 0xAA]) 0)
    Just BSS  -> moveAhead n
    Nothing   -> noSection)

returnCurrentResults = do st <- get
                          return $ (reverse $ bstLines_acc st, bstSymbols st)

buildSymbolTable :: [Line] -> ([(Int, Line)], SymbolTable)
buildSymbolTable [] = ([], []) -- chain requires non-empty lists
buildSymbolTable src =
  evalState (chain $ map doLine src) startState 

chain = foldr1 (>>)
  
doLine :: Line -> SymTableBuild
doLine (Label s) = do addSymbol s
                      returnCurrentResults
doLine (CmdLine ".text" args) = case args of
  ((Num _ n):[]) -> do newSection n
                       setSectionType Text
                       returnCurrentResults
  _              -> fail "Huge fucking screwup"
doLine (CmdLine ".bss" args) = case args of
  ((Num _ n):[]) -> do newSection n
                       setSectionType BSS
                       returnCurrentResults
  _              -> fail "What the hell, brah"
doLine (CmdLine ".align" args) = case args of
  ((Num _ n):[]) -> get >>= (\st ->
    case bstAddr st of
      Nothing -> noSection
      Just addr -> do
        let align = (2^n - addr `mod` 2^n) `mod` 2^n
        dummyBytes align
        returnCurrentResults)
  _              -> fail "Eat a dick."
doLine l@(CmdLine ".ascii" args) = do
  checkSectionType Text
  case args of
    ((Str s):[]) -> do addLine l 0
                       returnCurrentResults
    _            -> fail "Ballsacks!"
doLine l@(CmdLine ".asciiz" args) = do
  checkSectionType Text
  case args of
    ((Str s):[]) -> do addLine l 0
                       returnCurrentResults
    _            -> fail "Ballsackz!"
doLine l@(CmdLine ".space" args) = do
  checkSectionType Text
  case args of
    ((Num _ n):[]) -> do dummyBytes n
                         returnCurrentResults
    _              -> fail "FUCKTARDO"
doLine (CmdLine ".comm" args) = case args of
  ((Symbol name):(Num _ size):[]) -> do
    checkSectionType BSS
    addSymbol name
    moveAhead size
    returnCurrentResults
  _ -> fail "args"
doLine l@(CmdLine ".byte" _) = do 
  checkSectionType Text
  addLine l 0
  returnCurrentResults
doLine l@(CmdLine ".half" _) = do 
  checkSectionType Text
  addLine l 1
  returnCurrentResults
doLine l  = do addLine l 2
               returnCurrentResults

byteSize :: Line -> Int
byteSize (Label _) = 0
byteSize (CmdLine ".byte" args) = length args
byteSize (CmdLine ".half" args) = length args * 2
byteSize (CmdLine ".word" args) = length args * 4
byteSize (CmdLine _ _)          = 4
