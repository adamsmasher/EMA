{-# OPTIONS_GHC -XFlexibleInstances #-}

module Ema where

import Assembler (Bytecode, assemble, byteSize, makeI, makeJ, makeR)
import Expr (evalExpr)
import MIPSConst
import Parser (ConstType(..), Include(..), Line(..), constDirective, include,
               label, num, parseInt, parseLine, parseOffsetBase, parseRegister,
               parses, stringLiteral)
import Register
import Section (Section(..), sections)
import SymTable (SymbolTable, buildSymbolTable)
import Util (readNum, trimLeft, w16, w32)

import Control.Arrow ((>>>))
import Control.Monad (liftM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (pack, readFile, unpack)
import Data.Char (isDigit, toLower)
import Data.Word (Word8)
import Text.ParserCombinators.Parsec (parse)

data AssemblyState a = Error String | OK a 

instance Monad AssemblyState where
  return = OK 
  (Error a) >>= _ = Error a
  (OK b) >>= k = k b
  fail = Error
   
loadFile :: String -> IO [String]
loadFile str = readFile str >>= (return . cleanupFile) >>= doIncludes

doIncludes :: [String] -> IO [String]
doIncludes src = liftM concat $ sequence $ map doInclude src

-- removes source lines containing include directives, replaces them with
-- the text of the requested file
doInclude :: String -> IO [String]
doInclude line = case parse include "" line of
  Left _                  -> return [line]
  Right (Incsrc filename) -> loadFile filename
  Right (Incbin filename) -> incBin filename

incBin :: String -> IO [String]
incBin file = ByteString.readFile file 
          >>= return . ((map makeByte) . ByteString.unpack)
              where makeByte n = ".byte " ++ (show n)

-- given the text of a file, assembleFile converts it to an
-- assembled binary
assembleFile :: Monad m => [String] -> m ByteString
assembleFile src = do ss <- sections src
                      let (ls, syms) = symTables ss
                      let (ls', symbolTable) = (concat ls, concat syms)
                      bytes <- assembleLines symbolTable ls'
                      return $ ByteString.pack $ bytes

-- silly little helper
symTables :: [Section] -> ([[(Int, String)]], [SymbolTable])
symTables ss = unzip $ (flip map) ss (\(Section start lines) ->
  buildSymbolTable start lines)

assembleLines :: Monad m => SymbolTable -> [(Int, String)] -> m [Word8]
assembleLines symbolTable ls = liftM concat $ sequence $ map asm ls
  where asm = assembleLine symbolTable

-- splits a file up into lines, removes leading whitespace, comments, and empty
-- lines
cleanupFile :: String -> [String]
cleanupFile = lines >>>
              map trimLeft >>>
              map stripComment >>>
              foldr splitLabels [] >>>
              filter (not . null)
              where splitLabels x xs = case parse label "" x of
                      Left _          -> x:xs
                      Right (l, rest) -> [l ++ ":", rest] ++ xs

-- comments begin with a #, carry on to end of line
stripComment :: String -> String
stripComment = takeWhile (/='#')

-- assembleLine takes a line of code and, if successful, assembles it into
-- a binary word representing an instruction
assembleLine :: Monad m => SymbolTable -> (Int, String) -> m [Word8]
assembleLine symbolTable (addr, str) = case parse constDirective "" str of
  Left _ -> parseLine str >>=
            toInstruction symbolTable addr >>=
            return . assemble
  Right (Byte bytes) -> mapM readNum bytes >>=
                        return . (map fromIntegral)
  Right (Half hws)   -> mapM readNum hws >>=
                        liftM concat . mapM (liftM reverse . w16) . map fromIntegral
  Right (Word words) -> mapM readNum words >>=
                        liftM concat . mapM (liftM reverse . w32) . map fromIntegral

toInstruction :: Monad m => SymbolTable -> Int -> Line -> m Bytecode
toInstruction symbolTable addr l = case l of
  Line "add"    params -> case params of
    (r1:r2:r3:[]) -> do rd <- parseRegister r1
                        rs <- parseRegister r2
                        rt <- parseRegister r3
                        makeR 0 rs rt rd 0 addFunc
    _             -> fail "Incorrect number of arguments to 'add'"
  Line "addi"   params -> case params of
    (r1:r2:imm:[]) -> do rt <- parseRegister r1
                         rs <- parseRegister r2
                         n  <- eval imm
                         makeI addiOp rs rt n
  Line "addiu"  params -> case params of
    (r1:r2:imm:[]) -> do rt <- parseRegister r1
                         rs <- parseRegister r2
                         n  <- eval imm
                         makeI addiuOp rs rt n
  Line "addu"   params -> case params of
    (r1:r2:r3:[]) -> do rd <- parseRegister r1
                        rs <- parseRegister r2
                        rt <- parseRegister r3
                        makeR 0 rs rt rd 0 adduFunc
  Line "and"    params -> case params of
    (r1:r2:r3:[]) -> do rd <- parseRegister r1
                        rs <- parseRegister r2
                        rt <- parseRegister r3
                        makeR 0 rs rt rd 0 andFunc
  Line "andi"   params -> case params of
    (r1:r2:imm:[]) -> do rt <- parseRegister r1
                         rs <- parseRegister r2
                         n  <- eval imm
                         makeI andiOp rs rt n
  Line "beq"    params -> case params of
    (r1:r2:off:[]) -> do rs <- parseRegister r1
                         rt <- parseRegister r2
                         n  <- eval off
                         makeI beqOp rs rt (n - (addr+4))
  Line "bgez"   params -> case params of
    (r:off:[]) -> do rs <- parseRegister r
                     n  <- eval off
                     makeI regimm rs bgezFunc (n - (addr+4))
  Line "bgezal" params -> case params of
    (r:off:[]) -> do rs <- parseRegister r
                     n  <- eval off
                     makeI regimm rs bgezalFunc (n - (addr+4))
  Line "bgtz"   params -> case params of
    (r:off:[]) -> do rs <- parseRegister r
                     n  <- eval off
                     makeI bgtzOp rs 0 (n - (addr+4))
  Line "blez"   params -> case params of
    (r:off:[]) -> do rs <- parseRegister r
                     n  <- eval off
                     makeI blezOp rs 0 (n - (addr+4))
  Line "bltz"   params -> case params of
    (r:off:[]) -> do rs <- parseRegister r
                     n  <- eval off
                     makeI regimm rs bltzFunc (n - (addr+4))
  Line "bltzal" params -> case params of
    (r:off:[]) -> do rs <- parseRegister r
                     n  <- eval off
                     makeI regimm rs bltzalFunc (n - (addr+4))
  Line "bne"    params -> case params of
    (r1:r2:off:[]) -> do rs <- parseRegister r1
                         rt <- parseRegister r2
                         n  <- eval off
                         makeI bneOp rs rt (n - (addr+4))
  Line "div"    params -> case params of
    (r1:r2:[]) -> do rs <- parseRegister r1
                     rt <- parseRegister r2
                     makeR special rs rt 0 0 divFunc
  Line "divu"   params -> case params of
    (r1:r2:[]) -> do rs <- parseRegister r1
                     rt <- parseRegister r2
                     makeR special rs rt 0 0 divuFunc
  Line "j"      params -> case params of
    (target:[]) -> do n <- eval target
                      makeJ jOp n
  Line "jal"    params -> case params of
    (target:[]) -> do n <- eval target
                      makeJ jalOp n
  Line "jalr"   params -> case params of
    (r1:r2:[]) -> do rd <- parseRegister r1
                     rs <- parseRegister r2
                     makeR special rs 0 rd 0 jalrFunc
  Line "lb"     params -> case params of
    (r:offsetBase:[]) -> do rt <- parseRegister r
                            (off, base) <- parseOffsetBase offsetBase
                            makeI lbOp base rt off
  Line "lbu"    params -> case params of
    (r:offsetBase:[]) -> do rt <- parseRegister r
                            (off, base) <- parseOffsetBase offsetBase
                            makeI lbuOp base rt off
  Line "lh"     params -> case params of
    (r:offsetBase:[]) -> do rt <- parseRegister r
                            (off, base) <- parseOffsetBase offsetBase
                            makeI lhOp base rt off
  Line "lhu"    params -> case params of
    (r:offsetBase:[]) -> do rt <- parseRegister r
                            (off, base) <- parseOffsetBase offsetBase
                            makeI lhuOp base rt off
  Line "lui"    params -> case params of
    (r:imm:[]) -> do rt <- parseRegister r
                     n <- eval imm
                     makeI luiOp 0 rt n
  Line "lw"     params -> case params of
    (r:offsetBase:[]) -> do rt <- parseRegister r
                            (off, base) <- parseOffsetBase offsetBase
                            makeI lwOp base rt off 
  Line "lwl"    params -> case params of
    (r:offsetBase:[]) -> do rt <- parseRegister r
                            (off, base) <- parseOffsetBase offsetBase
                            makeI lwlOp base rt off
  Line "lwr"    params -> case params of
    (r:offsetBase:[]) -> do rt <- parseRegister r
                            (off, base) <- parseOffsetBase offsetBase
                            makeI lwrOp base rt off
  Line "mfhi"   params -> case params of
    (r:[]) -> do rd <- parseRegister r
                 makeR special 0 0 rd 0 mfhiFunc
  Line "mflo"   params -> case params of
    (r:[]) -> do rd <- parseRegister r
                 makeR special 0 0 rd 0 mfloFunc
  Line "mthi"   params -> case params of
    (r:[]) -> do rs <- parseRegister r
                 makeR special rs 0 0 0 mthiFunc
  Line "mtlo"   params -> case params of
    (r:[]) -> do rs <- parseRegister r
                 makeR special rs 0 0 0 mtloFunc
  Line "mult"   params -> case params of
    (r1:r2:[]) -> do rs <- parseRegister r1
                     rt <- parseRegister r2
                     makeR special rs rt 0 0 multFunc
  Line "multu"  params -> case params of
    (r1:r2:[]) -> do rs <- parseRegister r1
                     rt <- parseRegister r2
                     makeR special rs rt 0 0 multuFunc
  Line "nor"    params -> case params of
    (r1:r2:r3:[]) -> do rd <- parseRegister r1
                        rs <- parseRegister r2
                        rt <- parseRegister r3
                        makeR special rs rt rd 0 norFunc
  Line "or"     params -> case params of
    (r1:r2:r3:[]) -> do rd <- parseRegister r1
                        rs <- parseRegister r2
                        rt <- parseRegister r3
                        makeR special rs rt rd 0 orFunc
  Line "ori"    params -> case params of
    (r1:r2:imm:[]) -> do rt <- parseRegister r1
                         rs <- parseRegister r2
                         n  <- eval imm
                         makeI oriOp rs rt n
  Line "sb"     params -> case params of
    (r:offsetBase:[]) -> do rt <- parseRegister r
                            (off, base) <- parseOffsetBase offsetBase
                            makeI sbOp base rt off
  Line "sh"     params -> case params of
    (r:offsetBase:[]) -> do rt <- parseRegister r
                            (off, base) <- parseOffsetBase offsetBase
                            makeI shOp base rt off
  Line "sll"    params -> case params of
    (r1:r2:sa:[]) -> do rd <- parseRegister r1
                        rt <- parseRegister r2
                        n  <- eval sa
                        makeR special 0 rt rd n sllFunc
  Line "sllv"   params -> case params of
    (r1:r2:r3:[]) -> do rd <- parseRegister r1
                        rt <- parseRegister r2
                        rs <- parseRegister r3
                        makeR special rs rt rd 0 sllvFunc
  Line "slt"    params -> case params of
    (r1:r2:r3:[]) -> do rd <- parseRegister r1
                        rs <- parseRegister r2
                        rt <- parseRegister r3
                        makeR special rs rt rd 0 sltFunc
  Line "slti"   params -> case params of
    (r1:r2:imm:[]) -> do rt <- parseRegister r1
                         rs <- parseRegister r2
                         n  <- eval imm
                         makeI sltiOp rs rt n
  Line "sltiu"  params -> case params of
    (r1:r2:imm:[]) -> do rt <- parseRegister r1
                         rs <- parseRegister r2
                         n  <- eval imm
                         makeI sltiuOp rs rt n
  Line "sltu"   params -> case params of
    (r1:r2:r3:[]) -> do rd <- parseRegister r1
                        rs <- parseRegister r2
                        rt <- parseRegister r3
                        makeR special rs rt rd 0 sltuFunc
  Line "sra"    params -> case params of
    (r1:r2:sa:[]) -> do rd <- parseRegister r1
                        rt <- parseRegister r2
                        n  <- eval sa
                        makeR special 0 rt rd n sraFunc
  Line "srav"   params -> case params of
    (r1:r2:r3:[]) -> do rd <- parseRegister r1
                        rt <- parseRegister r2
                        rs <- parseRegister r3
                        makeR special rs rt rd 0 sravFunc
  Line "srl"    params -> case params of
    (r1:r2:sa:[]) -> do rd <- parseRegister r1
                        rt <- parseRegister r2
                        n  <- eval sa
                        makeR special 0 rt rd n srlFunc
  Line "srlv"   params -> case params of
    (r1:r2:r3:[]) -> do rd <- parseRegister r1
                        rt <- parseRegister r2
                        rs <- parseRegister r3
                        makeR special rs rt rd 0 srlvFunc
  Line "sub"    params -> case params of
    (r1:r2:r3:[]) -> do rd <- parseRegister r1
                        rs <- parseRegister r2
                        rt <- parseRegister r3
                        makeR special rs rt rd 0 subFunc
  Line "subu"   params -> case params of
    (r1:r2:r3:[]) -> do rd <- parseRegister r1
                        rs <- parseRegister r2
                        rt <- parseRegister r3
                        makeR special rs rt rd 0 subuFunc
  Line "sw"     params -> case params of
    (r:offsetBase:[]) -> do rt <- parseRegister r
                            (off, base) <- parseOffsetBase offsetBase
                            makeI swOp base rt off
  Line "swl"    params -> case params of
    (r:offsetBase:[]) -> do rt <- parseRegister r
                            (off, base) <- parseOffsetBase offsetBase
                            makeI swlOp base rt off
  Line "swr"    params -> case params of
    (r:offsetBase:[]) -> do rt <- parseRegister r
                            (off, base) <- parseOffsetBase offsetBase
                            makeI swrOp base rt off
  Line "xor"    params -> case params of
    (r1:r2:r3:[]) -> do rd <- parseRegister r1
                        rs <- parseRegister r2
                        rt <- parseRegister r3
                        makeR special rs rt rd 0 xorFunc
  Line "xori"   params -> case params of
    (r1:r2:imm:[]) -> do rs <- parseRegister r1
                         rt <- parseRegister r2
                         n  <- eval imm
                         makeI xoriOp rs rt n
  Line i _         -> fail $ "unknown instruction " ++ i
  where eval = evalExpr symbolTable
