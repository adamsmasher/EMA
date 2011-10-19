module Pass2 (SymbolTable, buildSymbolTable) where

import Parser (Line(..), Expr(..), NumType(..))
import Util (doTimes, invalidArgs)

import Control.Monad.State (State(..), get, put, evalState)

type SymbolTable = [(String, Expr)]

-- our sym table build is a process that stores its state in a BuildST
-- and its result is a pair of the remaining lines (strings + addresses) and a
-- symbol table
type SymTableBuild = State BuildST ([(Integer, Line)], SymbolTable)
data BuildST = BuildST { bstLines_acc   :: [(Integer, Line)],
                         bstSymbols     :: SymbolTable,
                         bstSectionType :: Maybe SectionType,
                         bstAddr        :: Maybe Integer }
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
  Just addr -> addConstant s (Num Hex addr))

addConstant :: String -> Expr -> SymTableBuild
addConstant s c = get >>= (\st -> let symTable = bstSymbols st in
  case lookup s symTable of
    Nothing -> do put st { bstSymbols = (s, c):symTable }
                  returnCurrentResults
    Just _  -> fail $ "Duplicate symbol " ++ s)

dummyBytes :: Integer -> SymTableBuild
dummyBytes n = get >>= (\st -> case bstSectionType st of
    Just Text -> do doTimes n (\_ -> addLine (CmdLine ".byte" [Num Hex 0xAA]) 0)
                    returnCurrentResults
    Just BSS  -> moveAhead n >> returnCurrentResults
    Nothing   -> noSection)

returnCurrentResults = do st <- get
                          return $ (reverse $ bstLines_acc st, bstSymbols st)

buildSymbolTable :: [Line] -> ([(Integer, Line)], SymbolTable)
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
  []             -> get >>= (\st ->
    case (bstAddr st) of
      Nothing   -> fail "No starting address for section"
      Just addr -> do newSection addr
                      setSectionType Text
                      returnCurrentResults)
  _              -> invalidArgs ".text"
doLine (CmdLine ".define" args) = case args of
  ((Symbol name):e:[]) -> do addConstant name e
                             returnCurrentResults
  _ -> invalidArgs ".define"
doLine (CmdLine ".bss" args) = case args of
  ((Num _ n):[]) -> do newSection n
                       setSectionType BSS
                       returnCurrentResults
  []             -> get >>= (\st ->
    case (bstAddr st) of
      Nothing   -> fail "No starting address for section"
      Just addr -> do newSection addr
                      setSectionType BSS
                      returnCurrentResults)
  _              -> invalidArgs ".bss"
doLine (CmdLine ".align" args) = case args of
  ((Num _ n):[]) -> get >>= (\st ->
    case bstAddr st of
      Nothing -> noSection
      Just addr -> do
        let align = (2^n - addr `mod` 2^n) `mod` 2^n
        dummyBytes align
        returnCurrentResults)
  _              -> invalidArgs ".align"
doLine l@(CmdLine ".ascii" args) = do
  checkSectionType Text
  case args of
    ((Str s):[]) -> do addLine l 0
                       returnCurrentResults
    _            -> invalidArgs ".ascii"
doLine l@(CmdLine ".asciiz" args) = do
  checkSectionType Text
  case args of
    ((Str s):[]) -> do addLine l 0
                       returnCurrentResults
    _            -> invalidArgs ".asciiz"
doLine l@(CmdLine ".space" args) = do
  checkSectionType Text
  case args of
    ((Num _ n):[]) -> do dummyBytes n
                         returnCurrentResults
    _              -> invalidArgs ".space"
doLine (CmdLine ".comm" args) = case args of
  ((Symbol name):(Num _ size):[]) -> do
    checkSectionType BSS
    addSymbol name
    moveAhead size
    returnCurrentResults
  _ -> invalidArgs ".comm"
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

byteSize :: Line -> Integer
byteSize (Label _) = 0
byteSize (CmdLine ".byte" args) = fromIntegral $ length args
byteSize (CmdLine ".half" args) = fromIntegral $ length args * 2
byteSize (CmdLine ".word" args) = fromIntegral $ length args * 4
byteSize (CmdLine _ _)          = 4
