module Main where

import Ema (AssemblyState(..), assembleFile, loadFile)

import qualified Data.ByteString as ByteString (writeFile)
import System (getArgs)
import System.Exit (exitFailure)

main = do args <- getArgs
          if length args /= 2 then usage
             else assembleToFile (args !! 0) (args !! 1)

usage = putStrLn "USAGE: ema <source-file> <output-file>"

assembleToFile :: String -> String -> IO ()
assembleToFile inFilename outFilename = do
  src <- loadFile inFilename
  bin <- catchParseError src
  ByteString.writeFile outFilename bin
  where catchParseError file = case assembleFile file of
          Error err -> do putStrLn $ "Error assembling " ++ inFilename
                          putStrLn err
                          exitFailure
          OK bin    -> return bin

