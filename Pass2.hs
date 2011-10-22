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

newSection :: Integer -> State BuildST ()
newSection s = get >>= (\st -> put st {bstAddr = (Just s)})

noSection = fail "all code must reside within a .text or .bss section"
invalidSection = fail "line not valid in current section type"
checkSectionType :: SectionType -> State BuildST ()
checkSectionType t = get >>= (\st ->
  case (bstSectionType st) == Just t of
    True  -> return ()
    False -> invalidSection)

getAddr = get >>= (\st -> case bstAddr st of
  Nothing -> noSection
  Just addr -> return addr)

setSectionType t = get >>= (\st -> put st { bstSectionType = Just t })

moveAhead n = do 
  addr <- getAddr
  st <- get
  put st {bstAddr = Just $ addr + n}

-- a is alignment (0 is no align, 1 is half, 2 is word...)
addLine l a = do
    addr <- getAddr
    let align = (2^a - addr `mod` 2^a) `mod` 2^a
    dummyBytes align 0
    st <- get
    put st {bstLines_acc = ((addr + align, l):(bstLines_acc st))}
    moveAhead (byteSize l)

addSymbol s = getAddr >>= (\addr -> addConstant s (Num Hex addr))

addConstant s c = get >>= (\st -> let symTable = bstSymbols st in
  case lookup s symTable of
    Nothing -> put st { bstSymbols = (s, c):symTable }
    Just _  -> fail $ "Duplicate symbol " ++ s)

dummyBytes n f = get >>= (\st -> case bstSectionType st of
    Just Text -> do doTimes n (\_ -> addLine (CmdLine ".byte" [Num Hex f]) 0)
    Just BSS  -> moveAhead n 
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
  []             -> do newSection =<<  getAddr
                       setSectionType Text
                       returnCurrentResults
  _              -> invalidArgs ".text"
doLine (CmdLine ".define" args) = case args of
  ((Symbol name):e:[]) -> do addConstant name e
                             returnCurrentResults
  _ -> invalidArgs ".define"
doLine (CmdLine ".bss" args) = case args of
  ((Num _ n):[]) -> do newSection n
                       setSectionType BSS
                       returnCurrentResults
  []             -> do newSection =<< getAddr
                       setSectionType BSS
                       returnCurrentResults
  _              -> invalidArgs ".bss"
doLine (CmdLine ".align" args) = case args of
  ((Num _ n):[]) -> do addr <- getAddr
                       let align = (2^n - addr `mod` 2^n) `mod` 2^n
                       dummyBytes align 0
                       returnCurrentResults
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
doLine (CmdLine ".padto" args) = do
  case args of
    ((Num _ n):(Num _ f):[]) -> do
      padding <- getAddr >>= return . (n-)
      dummyBytes padding f
      returnCurrentResults
    ((Num _ n):[]) -> do
      padding <- getAddr >>= return . (n-)
      dummyBytes padding 0xAA
      returnCurrentResults
    _              -> invalidArgs ".padto"
doLine (CmdLine ".space" args) = do
  case args of
    ((Num _ n):(Num _ f):[]) -> do dummyBytes n f
                                   returnCurrentResults
    ((Num _ n):[]) -> do dummyBytes n 0xAA
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
byteSize (CmdLine ".ascii" ((Str s):[]))  = fromIntegral $ length s
byteSize (CmdLine ".asciiz" ((Str s):[]))  = fromIntegral $ length s + 1
byteSize (CmdLine ".byte" args) = fromIntegral $ length args
byteSize (CmdLine ".half" args) = fromIntegral $ length args * 2
byteSize (CmdLine ".word" args) = fromIntegral $ length args * 4
byteSize (CmdLine _ _)          = 4
