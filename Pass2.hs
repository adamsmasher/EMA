module Pass2 (SymbolTable, buildSymbolTable) where

import ErrorHandling (AssemblyState(..), AssemblyStateT(..), invalidArgs)
import Parser (Line(..), LineType(..), Expr(..), NumType(..))

import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)
import Control.Monad.State (State(..), get, put, evalState)

type SymbolTable = [(String, Expr)]

-- our sym table build is a process that stores its state in a BuildST
-- and its result is a pair of the remaining lines (strings + addresses) and a
-- symbol table
type SymTableAction = AssemblyStateT (State BuildST) ()
type SymTableBuild = AssemblyStateT (State BuildST) ([(Integer, Line)], SymbolTable)
data BuildST = BuildST { bstLines_acc   :: [(Integer, Line)],
                         bstSymbols     :: SymbolTable,
                         bstSectionType :: Maybe SectionType,
                         bstAddr        :: Maybe Integer }
data SectionType = Text | BSS deriving Eq

startState = BuildST [] [] Nothing Nothing

newSection :: Integer -> SymTableAction
newSection s = lift get >>= (\st -> lift $ put st {bstAddr = (Just s)})

noSection = fail "all code must reside within a .text or .bss section"
invalidSection = fail "line not valid in current section type"
checkSectionType :: SectionType -> SymTableAction
checkSectionType t = lift get >>= (\st ->
  case (bstSectionType st) == Just t of
    True  -> return ()
    False -> invalidSection)

getAddr = lift get >>= (\st -> case bstAddr st of
  Nothing -> noSection
  Just addr -> return addr)

setSectionType :: SectionType -> SymTableAction
setSectionType t = lift get >>= (\st -> lift $ put st { bstSectionType = Just t })

moveAhead n = do 
  addr <- getAddr
  st <- lift get
  lift $ put st {bstAddr = Just $ addr + n}

-- a is alignment (0 is no align, 1 is half, 2 is word...)
addLine :: Line -> Integer -> SymTableAction
addLine l@(Line l' _) a = do
    addr <- getAddr
    let align = (2^a - addr `mod` 2^a) `mod` 2^a
    dummyBytes align 0
    st <- lift get
    lift $ put st {bstLines_acc = ((addr + align, l):(bstLines_acc st))}
    moveAhead (byteSize l')

addSymbol s = getAddr >>= (\addr -> addConstant s (Num Hex addr))

addConstant :: String -> Expr -> SymTableAction
addConstant s c = lift get >>= (\st -> let symTable = bstSymbols st in
  case lookup s symTable of
    Nothing -> lift $ put st { bstSymbols = (s, c):symTable }
    Just _  -> fail $ "Duplicate symbol " ++ s)


dummyBytes 0 _ = return ()
dummyBytes n f = lift get >>= (\st -> case bstSectionType st of
    Just Text -> addLine (Line (CmdLine ".byte" (replicate (fromIntegral n) $ Num Hex f)) 0) 0
    Just BSS  -> moveAhead n 
    Nothing   -> noSection)

returnCurrentResults = do st <- lift get
                          return $ (reverse $ bstLines_acc st, bstSymbols st)

buildSymbolTable :: [Line] -> AssemblyState ([(Integer, Line)], SymbolTable)
buildSymbolTable [] = return ([], []) -- chain requires non-empty lists
buildSymbolTable src =  
  evalState (runAssembly $ chain $ map doLine src) startState

chain = foldr1 (>>)
  
doLine :: Line -> SymTableBuild
doLine l@(Line l' lineNum) = case l' of
  Label s -> do addSymbol s
                returnCurrentResults
  CmdLine ".text" args -> case args of
    ((Num _ n):[]) -> do newSection n
                         setSectionType Text
                         returnCurrentResults
    []             -> do newSection =<<  getAddr
                         setSectionType Text
                         returnCurrentResults
    _              -> invalidArgs' ".text"
  CmdLine ".define" args -> case args of
    ((Symbol name):e:[]) -> do addConstant name e
                               returnCurrentResults
    _ -> invalidArgs' ".define"
  CmdLine ".bss" args -> case args of
    ((Num _ n):[]) -> do newSection n
                         setSectionType BSS
                         returnCurrentResults
    []             -> do newSection =<< getAddr
                         setSectionType BSS
                         returnCurrentResults
    _              -> invalidArgs' ".bss"
  CmdLine ".align" args -> case args of
    ((Num _ n):[]) -> do addr <- getAddr
                         let align = (2^n - addr `mod` 2^n) `mod` 2^n
                         dummyBytes align 0
                         returnCurrentResults
    _              -> invalidArgs' ".align"
  CmdLine ".ascii" args -> do
    checkSectionType Text
    case args of
      ((Str s):[]) -> do addLine l 0
                         returnCurrentResults
      _            -> invalidArgs' ".ascii"
  CmdLine ".asciiz" args -> do
    checkSectionType Text
    case args of
      ((Str s):[]) -> do addLine l 0
                         returnCurrentResults
      _            -> invalidArgs' ".asciiz"
  CmdLine ".padto" args -> do
    case args of
      ((Num _ n):(Num _ f):[]) -> do
        padding <- getAddr >>= return . (n-)
        case padding < 0 of
          True -> fail "Pad destination preceeds section counter"
          False -> do dummyBytes padding f
                      returnCurrentResults
      ((Num _ n):[]) -> do
        padding <- getAddr >>= return . (n-)
        case padding < 0 of
          True -> fail "Pad destination preceeds section counter"
          False -> do dummyBytes padding 0xAA
                      returnCurrentResults
      _              -> invalidArgs' ".padto"
  CmdLine ".space" args -> do
    case args of
      ((Num _ n):(Num _ f):[]) -> do dummyBytes n f
                                     returnCurrentResults
      ((Num _ n):[]) -> do dummyBytes n 0xAA
                           returnCurrentResults
      _              -> invalidArgs' ".space"
  CmdLine ".comm" args -> case args of
    ((Symbol name):(Num _ size):[]) -> do
      checkSectionType BSS
      addSymbol name
      moveAhead size
      returnCurrentResults
    _ -> invalidArgs' ".comm"
  CmdLine ".byte" _ -> do 
    checkSectionType Text
    addLine l 0
    returnCurrentResults
  CmdLine ".half" _ -> do 
    checkSectionType Text
    addLine l 1
    returnCurrentResults
  l' -> do addLine l 2
           returnCurrentResults
 where invalidArgs' s = AssemblyStateT . return $ invalidArgs s lineNum

byteSize :: LineType -> Integer
byteSize (Label _) = 0
byteSize (CmdLine ".ascii" ((Str s):[]))  = fromIntegral $ length s
byteSize (CmdLine ".asciiz" ((Str s):[]))  = fromIntegral $ length s + 1
byteSize (CmdLine ".byte" args) = fromIntegral $ length args
byteSize (CmdLine ".half" args) = fromIntegral $ length args * 2
byteSize (CmdLine ".word" args) = fromIntegral $ length args * 4
byteSize (CmdLine  _ _)          = 4
