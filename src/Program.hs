module Program
  (runProgram)
  where

import           Instruction
import           Memory

--import           Data.Binary.Strict
import           Data.Binary.Strict.Get

import           Data.ByteString (ByteString)

import           Control.Applicative
import           Control.Monad.State.Strict

import           Control.Lens


programWords :: ByteString -> [MachineWord]
programWords b = case fst $ runGet go b of
                  Right ws -> ws
                  Left s -> error s
  where
    go :: Get [MachineWord]
    go = {-# SCC "programWords.go" #-} do
      done <- isEmpty

      if done
        then return []
        else (:) <$> getWord32be <*> go
{-# INLINE programWords #-}

runProgram :: ByteString -> IO ()
runProgram =
  evalStateT spinCycle . loadMachineProgram . programWords

spinCycle :: StateT Machine IO ()
spinCycle = {-# SCC "spinCycle" #-} forever $ do
  currIp      <- use ip
  instruction <- use . to $ getMemElem 0 (fromIntegral currIp) -- use $ memoryElement 0 (fromIntegral currIp)
  runInstruction $ readInstruction instruction
  ip += 1
{-# INLINE spinCycle #-}
