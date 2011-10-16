module Ema where

import Assembler (Bytecode, assemble, makeI, makeJ, makeR)
import Eval (evalExpr)
import MIPSConst
import Parser (Line(..), Expr(..), NumType(..), parseFile)
import Pass2 (SymbolTable, buildSymbolTable)
import Util (w16, w32)

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
                                _            -> fail "Fuck you."
  CmdLine ".incbin" args -> case args of
                                ((Str f):[]) -> incBin f
                                _            -> fail "You suck."
  CmdLine _ _            -> return [line]

incBin :: String -> IO [Line]
incBin file = ByteString.readFile file
          >>= return . ((map makeByte) . ByteString.unpack)
              where makeByte n = CmdLine ".byte" [Num Dec $ fromIntegral n]

-- given the text of a file, assembleFile converts it to an
-- assembled binary
assembleFile :: Monad m => [Line] -> m ByteString
assembleFile src = do let (ls, symbolTable) = buildSymbolTable src
                      bytes <- assembleLines symbolTable ls
                      return $ ByteString.pack $ bytes

assembleLines :: Monad m => SymbolTable -> [(Int, Line)] -> m [Word8]
assembleLines symbolTable ls = liftM concat $ sequence $ map asm ls
  where asm = assembleLine symbolTable

-- assembleLine takes a line of code and, if successful, assembles it into
-- a binary word representing an instruction
assembleLine :: Monad m => SymbolTable -> (Int, Line) -> m [Word8]
assembleLine symbolTable (addr, l) = case l of
  CmdLine ".byte" bytes -> do bs <- sequence $ map (evalExpr symbolTable) bytes
                              return . (map fromIntegral) $ bs
  CmdLine ".half" halfs -> do hs <- sequence $ map (evalExpr symbolTable) halfs
                              liftM concat . mapM (liftM reverse . w16) $ hs
  CmdLine ".word" words -> do ws <- sequence $ map (evalExpr symbolTable) words
                              liftM concat . mapM (liftM reverse . w32) $ ws
  CmdLine ".ascii" ((Str s):[]) -> return $ map (fromIntegral . ord) s
  CmdLine ".asciiz"((Str s):[]) -> return $
    (map (fromIntegral . ord) s) ++ [(fromIntegral 0)]
  CmdLine _ _           -> toInstruction symbolTable addr l >>= 
                           return . assemble

toInstruction :: Monad m => SymbolTable -> Int -> Line -> m Bytecode
toInstruction symbolTable addr l = case l of
  CmdLine "add"    params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) -> makeR 0 rs rt rd 0 addFunc
    _ ->
      fail "Incorrect number of arguments to 'add'"
  CmdLine "addi"   params -> case params of
    ((Register rt):(Register rs):imm:[]) -> do 
      n  <- eval imm
      makeI addiOp rs rt n
  CmdLine "addiu"  params -> case params of
    ((Register rt):(Register rs):imm:[]) -> do 
      n  <- eval imm
      makeI addiuOp rs rt n
  CmdLine "addu"   params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) -> 
      makeR 0 rs rt rd 0 adduFunc
  CmdLine "and"    params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) ->
      makeR 0 rs rt rd 0 andFunc
  CmdLine "andi"   params -> case params of
    ((Register rt):(Register rs):imm:[]) -> do 
      n  <- eval imm
      makeI andiOp rs rt n
  CmdLine "beq"    params -> case params of
    ((Register rs):(Register rt):off:[]) -> do
      n  <- eval off
      makeI beqOp rs rt (n - (addr+4))
  CmdLine "bgez"   params -> case params of
    ((Register rs):off:[]) -> do 
                     n  <- eval off
                     makeI regimm rs bgezFunc (n - (addr+4))
  CmdLine "bgezal" params -> case params of
    ((Register rs):off:[]) -> do
                     n  <- eval off
                     makeI regimm rs bgezalFunc (n - (addr+4))
  CmdLine "bgtz"   params -> case params of
    ((Register rs):off:[]) -> do
      n  <- eval off
      makeI bgtzOp rs 0 (n - (addr+4))
  CmdLine "blez"   params -> case params of
    ((Register rs):off:[]) -> do
      n  <- eval off
      makeI blezOp rs 0 (n - (addr+4))
  CmdLine "bltz"   params -> case params of
    ((Register rs):off:[]) -> do
      n  <- eval off
      makeI regimm rs bltzFunc (n - (addr+4))
  CmdLine "bltzal" params -> case params of
    ((Register rs):off:[]) -> do
      n  <- eval off
      makeI regimm rs bltzalFunc (n - (addr+4))
  CmdLine "bne"    params -> case params of
    ((Register rs):(Register rt):off:[]) -> do
      n  <- eval off
      makeI bneOp rs rt (n - (addr+4))
  CmdLine "div"    params -> case params of
    ((Register rs):(Register rt):[]) -> 
      makeR special rs rt 0 0 divFunc
  CmdLine "divu"   params -> case params of
    ((Register rs):(Register rt):[]) ->
      makeR special rs rt 0 0 divuFunc
  CmdLine "j"      params -> case params of
    (target:[]) -> do n <- eval target
                      makeJ jOp n
  CmdLine "jal"    params -> case params of
    (target:[]) -> do n <- eval target
                      makeJ jalOp n
  CmdLine "jalr"   params -> case params of
    ((Register rd):(Register rs):[]) -> 
      makeR special rs 0 rd 0 jalrFunc
  CmdLine "lb"     params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI lbOp base rt off'
  CmdLine "lbu"    params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI lbuOp base rt off'
  CmdLine "lh"     params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI lhOp base rt off'
  CmdLine "lhu"    params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI lhuOp base rt off'
  CmdLine "lui"    params -> case params of
    ((Register rt):imm:[]) -> do
      n <- eval imm
      makeI luiOp 0 rt n
  CmdLine "lw"     params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI lwOp base rt off'
  CmdLine "lwl"    params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI lwlOp base rt off'
  CmdLine "lwr"    params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI lwrOp base rt off'
  CmdLine "mfhi"   params -> case params of
    ((Register rd):[]) ->
      makeR special 0 0 rd 0 mfhiFunc
  CmdLine "mflo"   params -> case params of
    ((Register rd):[]) ->
      makeR special 0 0 rd 0 mfloFunc
  CmdLine "mthi"   params -> case params of
    ((Register rs):[]) ->
      makeR special rs 0 0 0 mthiFunc
  CmdLine "mtlo"   params -> case params of
    ((Register rs):[]) ->
      makeR special rs 0 0 0 mtloFunc
  CmdLine "mult"   params -> case params of
    ((Register rs):(Register rt):[]) ->
      makeR special rs rt 0 0 multFunc
  CmdLine "multu"  params -> case params of
    ((Register rs):(Register rt):[]) ->
      makeR special rs rt 0 0 multuFunc
  CmdLine "nor"    params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) ->
      makeR special rs rt rd 0 norFunc
  CmdLine "or"     params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) ->
      makeR special rs rt rd 0 orFunc
  CmdLine "ori"    params -> case params of
    ((Register rt):(Register rs):imm:[]) -> do
      n  <- eval imm
      makeI oriOp rs rt n
  CmdLine "sb"     params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI sbOp base rt off'
  CmdLine "sh"     params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI shOp base rt off'
  CmdLine "sll"    params -> case params of
    ((Register rd):(Register rt):sa:[]) -> do
      n  <- eval sa
      makeR special 0 rt rd n sllFunc
  CmdLine "sllv"   params -> case params of
    ((Register rd):(Register rt):(Register rs):[]) ->
      makeR special rs rt rd 0 sllvFunc
  CmdLine "slt"    params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) ->
      makeR special rs rt rd 0 sltFunc
  CmdLine "slti"   params -> case params of
    ((Register rt):(Register rs):imm:[]) -> do
      n  <- eval imm
      makeI sltiOp rs rt n
  CmdLine "sltiu"  params -> case params of
    ((Register rt):(Register rs):imm:[]) -> do
      n  <- eval imm
      makeI sltiuOp rs rt n
  CmdLine "sltu"   params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) ->
      makeR special rs rt rd 0 sltuFunc
  CmdLine "sra"    params -> case params of
    ((Register rd):(Register rt):sa:[]) -> do
      n  <- eval sa
      makeR special 0 rt rd n sraFunc
  CmdLine "srav"   params -> case params of
    ((Register rd):(Register rt):(Register rs):[]) ->
      makeR special rs rt rd 0 sravFunc
  CmdLine "srl"    params -> case params of
    ((Register rd):(Register rt):sa:[]) -> do
      n  <- eval sa
      makeR special 0 rt rd n srlFunc
  CmdLine "srlv"   params -> case params of
    ((Register rd):(Register rt):(Register rs):[]) ->
      makeR special rs rt rd 0 srlvFunc
  CmdLine "sub"    params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) ->
      makeR special rs rt rd 0 subFunc
  CmdLine "subu"   params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) ->
      makeR special rs rt rd 0 subuFunc
  CmdLine "sw"     params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI swOp base rt off'
  CmdLine "swl"    params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI swlOp base rt off'
  CmdLine "swr"    params -> case params of
    ((Register rt):(OffsetBase off base):[]) -> do
      off' <- eval off
      makeI swrOp base rt off'
  CmdLine "xor"    params -> case params of
    ((Register rd):(Register rs):(Register rt):[]) -> 
      makeR special rs rt rd 0 xorFunc
  CmdLine "xori"   params -> case params of
    ((Register rs):(Register rt):imm:[]) -> do
      n  <- eval imm
      makeI xoriOp rs rt n
  CmdLine i _         -> fail $ "unknown instruction " ++ i
  where eval = evalExpr symbolTable
