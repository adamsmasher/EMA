module Ema where


import qualified Debug.Trace as Debug

import Assembler (Bytecode, assemble, makeBranch, makeI, makeJ, makeR)
import Eval (evalExpr)
import Macro (expandMacros)
import MIPSConst
import Parser (Line(..), Expr(..), NumType(..), parseFile)
import Pass2 (SymbolTable, buildSymbolTable)
import Util (invalidArgs, w16, w32)

import Control.Monad (liftM)
import Data.ByteString (ByteString)
import Data.Char (ord)
import qualified Data.ByteString as ByteString (pack, readFile, unpack)
import Data.Word (Word8)

data AssemblyState a = Error String | OK a 

instance Monad AssemblyState where
  return = OK 
  (Error a) >>= _ = Error a
  (OK b) >>= k = k b
  fail = Error
   
loadFile :: String -> IO [Line]
loadFile f = do src <- readFile f
                case parseFile f src of
                  Left err       -> fail $ (show err)
                  Right srcLines -> doIncludes srcLines

doIncludes :: [Line] -> IO [Line]
doIncludes src = liftM concat $ sequence $ map doInclude src

doInclude :: Line -> IO [Line]
doInclude line = case line of
  Label _                -> return [line]
  CmdLine ".incsrc" args -> case args of
                                ((Str f):[]) -> loadFile f 
                                _            -> invalidArgs ".incsrc"
  CmdLine ".incbin" args -> case args of
                                ((Str f):[]) -> incBin f
                                _            -> invalidArgs ".incbin"
  CmdLine _ _            -> return [line]

incBin :: String -> IO [Line]
incBin file = ByteString.readFile file
          >>= return . ((map makeByte) . ByteString.unpack)
              where makeByte n = CmdLine ".byte" [Num Dec $ fromIntegral n]

-- given the text of a file, assembleFile converts it to an
-- assembled binary
assembleFile :: Monad m => [Line] -> m ByteString
assembleFile src = do expandedSrc <- expandMacros src
                      let (finalSrc, symbolTable) = buildSymbolTable expandedSrc
                      bytes <- assembleLines symbolTable finalSrc
                      return $ ByteString.pack $ bytes

assembleLines :: Monad m => SymbolTable -> [(Integer, Line)] -> m [Word8]
assembleLines symbolTable ls = liftM concat $ sequence $ map asm ls
  where asm = assembleLine symbolTable

-- assembleLine takes a line of code and, if successful, assembles it into
-- a binary word representing an instruction
assembleLine :: Monad m => SymbolTable -> (Integer, Line) -> m [Word8]
assembleLine symbolTable (addr, l) = case l of
  CmdLine ".byte" bytes -> do bs <- sequence $ map (evalExpr symbolTable) bytes
                              return . (map fromIntegral) $ bs
  CmdLine ".half" halfs -> do hs <- sequence $ map (evalExpr symbolTable) halfs
                              liftM concat . mapM w16 $ hs
  CmdLine ".word" words -> do ws <- sequence $ map (evalExpr symbolTable) words
                              liftM concat . mapM w32 $ ws
  CmdLine ".ascii" ((Str s):[]) -> return $ map (fromIntegral . ord) s
  CmdLine ".asciiz"((Str s):[]) -> return $
    (map (fromIntegral . ord) s) ++ [(fromIntegral 0)]
  CmdLine _ _           -> toInstruction symbolTable addr l >>= 
                           return . assemble

toInstruction :: Monad m => SymbolTable -> Integer -> Line -> m Bytecode
toInstruction symbolTable addr l = case l of
  CmdLine "add"    params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) -> makeR 0 rs rt rd 0 addFunc
    _ -> invalidArgs "add"
  CmdLine "addi"   params -> case params of
    ((Register rt):(Register rs):imm:[]) -> do 
      n  <- eval imm
      makeI addiOp rs rt n
    _ -> invalidArgs "addi"
  CmdLine "addiu"  params -> case params of
    ((Register rt):(Register rs):imm:[]) -> do 
      n  <- eval imm
      makeI addiuOp rs rt n
    _ -> invalidArgs "addiu"
  CmdLine "addu"   params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) -> 
      makeR 0 rs rt rd 0 adduFunc
    _ -> invalidArgs "addu"
  CmdLine "and"    params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) ->
      makeR 0 rs rt rd 0 andFunc
    _ -> invalidArgs "and"
  CmdLine "andi"   params -> case params of
    ((Register rt):(Register rs):imm:[]) -> do 
      n  <- eval imm
      makeI andiOp rs rt n
    _ -> invalidArgs "andi"
  CmdLine "beq"    params -> case params of
    ((Register rs):(Register rt):off:[]) -> do
      n  <- eval off
      makeBranch beqOp rs rt n addr
    _ -> invalidArgs "beq"
  CmdLine "bgez"   params -> case params of
    ((Register rs):off:[]) -> do 
                     n  <- eval off
                     makeBranch regimm rs bgezFunc n addr
    _ -> invalidArgs "bgez"
  CmdLine "bgezal" params -> case params of
    ((Register rs):off:[]) -> do
                     n  <- eval off
                     makeBranch regimm rs bgezalFunc n addr
    _ -> invalidArgs "bgezal"
  CmdLine "bgtz"   params -> case params of
    ((Register rs):off:[]) -> do
      n  <- eval off
      makeBranch bgtzOp rs 0 n addr
    _ -> invalidArgs "bgtz"
  CmdLine "blez"   params -> case params of
    ((Register rs):off:[]) -> do
      n  <- eval off
      makeBranch blezOp rs 0 n addr
    _ -> invalidArgs "blez"
  CmdLine "bltz"   params -> case params of
    ((Register rs):off:[]) -> do
      n  <- eval off
      makeBranch regimm rs bltzFunc n addr
    _ -> invalidArgs "bltz"
  CmdLine "bltzal" params -> case params of
    ((Register rs):off:[]) -> do
      n  <- eval off
      makeBranch regimm rs bltzalFunc n addr
    _ -> invalidArgs "bltzal"
  CmdLine "bne"    params -> case params of
    ((Register rs):(Register rt):off:[]) -> do
      n  <- eval off
      makeBranch bneOp rs rt n addr
    _ -> invalidArgs "bne"
  CmdLine "div"    params -> case params of
    ((Register rs):(Register rt):[]) -> 
      makeR special rs rt 0 0 divFunc
    _ -> invalidArgs "div"
  CmdLine "divu"   params -> case params of
    ((Register rs):(Register rt):[]) ->
      makeR special rs rt 0 0 divuFunc
    _ -> invalidArgs "divu"
  CmdLine "j"      params -> case params of
    (target:[]) -> do n <- eval target
                      makeJ jOp n
    _ -> invalidArgs "j"
  CmdLine "jal"    params -> case params of
    (target:[]) -> do n <- eval target
                      makeJ jalOp n
    _ -> invalidArgs "jal"
  CmdLine "jalr"   params -> case params of
    ((Register rd):(Register rs):[]) -> 
      makeR special rs 0 rd 0 jalrFunc
    _ -> invalidArgs "jalr"
  CmdLine "jr"     params -> case params of
    ((Register rs):[]) -> makeR special rs 0 0 0 jrFunc
    _ -> invalidArgs "jr"
  CmdLine "lb"     params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI lbOp base rt off'
    _ -> invalidArgs "lb"
  CmdLine "lbu"    params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI lbuOp base rt off'
    _ -> invalidArgs "lbu"
  CmdLine "lh"     params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI lhOp base rt off'
    _ -> invalidArgs "lh"
  CmdLine "lhu"    params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI lhuOp base rt off'
    _ -> invalidArgs "lhu"
  CmdLine "lui"    params -> case params of
    ((Register rt):imm:[]) -> do
      n <- eval imm
      makeI luiOp 0 rt n
    _ -> invalidArgs "lui"
  CmdLine "lw"     params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI lwOp base rt off'
    _ -> invalidArgs "lw"
  CmdLine "lwl"    params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI lwlOp base rt off'
    _ -> invalidArgs "lwl"
  CmdLine "lwr"    params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI lwrOp base rt off'
    _ -> invalidArgs "lwr"
  CmdLine "mfhi"   params -> case params of
    ((Register rd):[]) ->
      makeR special 0 0 rd 0 mfhiFunc
    _ -> invalidArgs "mfhi"
  CmdLine "mflo"   params -> case params of
    ((Register rd):[]) ->
      makeR special 0 0 rd 0 mfloFunc
    _ -> invalidArgs "mflo"
  CmdLine "mthi"   params -> case params of
    ((Register rs):[]) ->
      makeR special rs 0 0 0 mthiFunc
    _ -> invalidArgs "mthi"
  CmdLine "mtlo"   params -> case params of
    ((Register rs):[]) ->
      makeR special rs 0 0 0 mtloFunc
    _ -> invalidArgs "mtlo"
  CmdLine "mult"   params -> case params of
    ((Register rs):(Register rt):[]) ->
      makeR special rs rt 0 0 multFunc
    _ -> invalidArgs "mult"
  CmdLine "multu"  params -> case params of
    ((Register rs):(Register rt):[]) ->
      makeR special rs rt 0 0 multuFunc
    _ -> invalidArgs "multu"
  CmdLine "nor"    params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) ->
      makeR special rs rt rd 0 norFunc
    _ -> invalidArgs "nor"
  CmdLine "or"     params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) ->
      makeR special rs rt rd 0 orFunc
    _ -> invalidArgs "or"
  CmdLine "ori"    params -> case params of
    ((Register rt):(Register rs):imm:[]) -> do
      n  <- eval imm
      makeI oriOp rs rt n
    _ -> invalidArgs "ori"
  CmdLine "sb"     params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI sbOp base rt off'
    _ -> invalidArgs "sb"
  CmdLine "sh"     params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI shOp base rt off'
    _ -> invalidArgs "sh"
  CmdLine "sll"    params -> case params of
    ((Register rd):(Register rt):sa:[]) -> do
      n  <- eval sa
      makeR special 0 rt rd n sllFunc
    _ -> invalidArgs "sll"
  CmdLine "sllv"   params -> case params of
    ((Register rd):(Register rt):(Register rs):[]) ->
      makeR special rs rt rd 0 sllvFunc
    _ -> invalidArgs "sllv"
  CmdLine "slt"    params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) ->
      makeR special rs rt rd 0 sltFunc
    _ -> invalidArgs "slt"
  CmdLine "slti"   params -> case params of
    ((Register rt):(Register rs):imm:[]) -> do
      n  <- eval imm
      makeI sltiOp rs rt n
    _ -> invalidArgs "slti"
  CmdLine "sltiu"  params -> case params of
    ((Register rt):(Register rs):imm:[]) -> do
      n  <- eval imm
      makeI sltiuOp rs rt n
    _ -> invalidArgs "sltiu"
  CmdLine "sltu"   params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) ->
      makeR special rs rt rd 0 sltuFunc
    _ -> invalidArgs "sltu"
  CmdLine "sra"    params -> case params of
    ((Register rd):(Register rt):sa:[]) -> do
      n  <- eval sa
      makeR special 0 rt rd n sraFunc
    _ -> invalidArgs "sra"
  CmdLine "srav"   params -> case params of
    ((Register rd):(Register rt):(Register rs):[]) ->
      makeR special rs rt rd 0 sravFunc
    _ -> invalidArgs "srav"
  CmdLine "srl"    params -> case params of
    ((Register rd):(Register rt):sa:[]) -> do
      n  <- eval sa
      makeR special 0 rt rd n srlFunc
    _ -> invalidArgs "srl"
  CmdLine "srlv"   params -> case params of
    ((Register rd):(Register rt):(Register rs):[]) ->
      makeR special rs rt rd 0 srlvFunc
    _ -> invalidArgs "srlv"
  CmdLine "sub"    params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) ->
      makeR special rs rt rd 0 subFunc
    _ -> invalidArgs "sub"
  CmdLine "subu"   params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) ->
      makeR special rs rt rd 0 subuFunc
    _ -> invalidArgs "subu"
  CmdLine "sw"     params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI swOp base rt off'
    _ -> invalidArgs "sw"
  CmdLine "swl"    params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI swlOp base rt off'
    _ -> invalidArgs "swl"
  CmdLine "swr"    params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI swrOp base rt off'
    _ -> invalidArgs "swr"
  CmdLine "xor"    params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) -> 
      makeR special rs rt rd 0 xorFunc
    _ -> invalidArgs "xor"
  CmdLine "xori"   params -> case params of
    ((Register rs):(Register rt):imm:[]) -> do
      n  <- eval imm
      makeI xoriOp rs rt n
    _ -> invalidArgs "xori"
  CmdLine i _         -> fail $ "unknown instruction " ++ i
  where eval = evalExpr symbolTable
