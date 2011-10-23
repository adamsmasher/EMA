module Main where

import Ema (assembleFile, loadFile)
import ErrorHandling (AssemblyState(..), AssemblyStateT(..))

import qualified Data.ByteString as ByteString (writeFile)
import System (getArgs)
import System.Exit (exitFailure)

main = do args <- getArgs
          if length args /= 2 then usage
             else assembleToFile (args !! 0) (args !! 1)

usage = putStrLn "USAGE: ema <source-file> <output-file>"

assembleToFile :: String -> String -> IO ()
assembleToFile inFilename outFilename = do
  src <- catchErrorT $ loadFile inFilename
  bin <- catchError  $ assembleFile src
  ByteString.writeFile outFilename bin
  where catchError a = case a of
          Error err lineNum -> do
            putStrLn $ "Error assembling " ++ inFilename
            case lineNum of
              Just n  -> putStrLn $ "On line " ++ (show n)
              Nothing -> return ()
            putStrLn err
            exitFailure
          OK bin    -> return bin
        catchErrorT a = runAssembly a >>= catchError

