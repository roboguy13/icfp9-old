module Main
  where

import           Program

import qualified Data.ByteString as BS

import           System.Environment


main :: IO ()
main = do
  [programName] <- getArgs

  program <- BS.readFile programName

  runProgram program
