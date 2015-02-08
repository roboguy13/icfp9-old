{-# LANGUAGE Rank2Types, TemplateHaskell, LambdaCase, BangPatterns, CPP #-}
module Instruction
  (readOpcode
  ,readInstruction
  ,runInstruction
  )
  where

import           Memory

import           Control.Lens

import           Data.Bits
import           Data.Word (Word32, Word8)
import           Data.Char (chr, ord)

import           Control.Monad.State.Strict

import           System.Exit (exitSuccess)

import           System.Mem


import Numeric (showHex)

#if DEBUG
import Debug.Trace
#else
trace :: String -> a -> a
trace _ = id

traceShowId :: a -> a
traceShowId = id
#endif

{-
traceShow :: a -> b -> b
traceShow = flip const

traceShowId :: a -> a
traceShowId = id
-}


data RegisterNumTriplet = RegisterNumTriplet
  { _aRegNum :: RegisterNum
  , _bRegNum :: RegisterNum
  , _cRegNum :: RegisterNum
  }
  deriving Show
makeLenses ''RegisterNumTriplet

data Instruction =
  -- *** Standard Operators ***
    CondMove       RegisterNumTriplet

  | ArrayIndex     RegisterNumTriplet
  | ArrayAmendment RegisterNumTriplet

  | Addition       RegisterNumTriplet
  | Multiplication RegisterNumTriplet
  | Division       RegisterNumTriplet
  | NotAnd         RegisterNumTriplet

  -- *** Other Operators ***
  | Halt

  | Allocation     RegisterNum RegisterNum
  | Abandoment     RegisterNum
  | Output         RegisterNum
  | Input          RegisterNum
  | LoadProgram    RegisterNum RegisterNum

  -- *** Special Operators ***
  | Orthography    RegisterNum MachineWord

instance Show Instruction where
  show (CondMove       triplet) = "CondMove (" ++ show triplet ++ ")"
  show (ArrayIndex     triplet) = "ArrayIndex (" ++ show triplet ++ ")"
  show (ArrayAmendment triplet) = "ArrayAmendment (" ++ show triplet ++ ")"
  show (Addition       triplet) = "Addition (" ++ show triplet ++ ")"
  show (Multiplication triplet) = "Multiplication (" ++ show triplet ++ ")"
  show (Division       triplet) = "Division (" ++ show triplet ++ ")"
  show (NotAnd         triplet) = "NotAnd (" ++ show triplet ++ ")"
  show  Halt                    = "Halt"

  show (Allocation  a b       ) = "Allocation 0x" ++ showHex a "" ++ " 0x" ++ showHex b ""
  show (Abandoment  a         ) = "Abandoment 0x" ++ showHex a ""
  show (Output      a         ) = "Output 0x" ++ showHex a ""
  show (Input       a         ) = "Input 0x" ++ showHex a ""
  show (LoadProgram a b       ) = "LoadProgram 0x" ++ showHex a "" ++ " 0x" ++ showHex b ""
  show (Orthography a val     ) = "Orthography 0x" ++ showHex a "" ++ " 0x" ++ showHex val ""

regLens :: Lens' RegisterNumTriplet RegisterNum -> RegisterNumTriplet -> Lens' Machine Register
regLens numLens triplet = {-# SCC "regLens" #-} register (triplet ^. numLens)
{-# INLINE regLens #-}

aReg, bReg, cReg :: RegisterNumTriplet -> Lens' Machine Register
aReg = regLens aRegNum
bReg = regLens bRegNum
cReg = regLens cRegNum
{-# INLINE aReg #-}
{-# INLINE bReg #-}
{-# INLINE cReg #-}

readRegisterTriplet :: MachineWord -> RegisterNumTriplet
readRegisterTriplet w =
  let !c = fromIntegral $  w             .&. 7
      !b = fromIntegral $ (w `shiftR` 3) .&. 7
      !a = fromIntegral $ (w `shiftR` 6) .&. 7
  in
  RegisterNumTriplet a b c

readOpcode :: MachineWord -> Word8
readOpcode w =
  fromIntegral $ (w `shiftR` 28) .&. 0xf

readOrthographyArgs :: MachineWord -> (RegisterNum, MachineWord)
readOrthographyArgs w =
  let !value =  w              .&. (2^(25::Int) - 1)
      !a     = (w `shiftR` 25) .&. 7
  in
  (fromIntegral a, fromIntegral value)

readInstruction :: MachineWord -> Instruction
readInstruction w = {-# SCC "readInstruction" #-}
  case readOpcode w of
    0  ->         CondMove       (readRegisterTriplet w)
    1  ->         ArrayIndex     (readRegisterTriplet w)
    2  ->         ArrayAmendment (readRegisterTriplet w)
    3  ->         Addition       (readRegisterTriplet w)
    4  ->         Multiplication (readRegisterTriplet w)
    5  ->         Division       (readRegisterTriplet w)
    6  ->         NotAnd         (readRegisterTriplet w)

    7  ->         Halt

    8  ->         Allocation     (readRegisterTriplet w ^. bRegNum) (readRegisterTriplet w ^. cRegNum)
    9  ->         Abandoment                                        (readRegisterTriplet w ^. cRegNum)
    10 ->         Output                                            (readRegisterTriplet w ^. cRegNum)
    11 ->         Input                                             (readRegisterTriplet w ^. cRegNum)
    12 ->         LoadProgram    (readRegisterTriplet w ^. bRegNum) (readRegisterTriplet w ^. cRegNum)

    13 -> uncurry Orthography    (readOrthographyArgs w)
    n  -> error $ "Illegal instruction: " ++ show n
{-# INLINE readInstruction #-}

runInstruction :: Instruction -> StateT Machine IO ()
runInstruction = {-# SCC "runInstruction" #-} (. traceShowId) $
  \case
    CondMove       t  -> condMove       t
    ArrayIndex     t  -> arrayIndex     t
    ArrayAmendment t  -> arrayAmendment t
    Addition       t  -> addition       t
    Multiplication t  -> multiplication t
    Division       t  -> division       t
    NotAnd         t  -> notAnd         t

    Halt              -> halt

    Allocation  b c   -> allocation  b c
    Abandoment    c   -> abandonment   c
    Output        c   -> output        c
    Input         c   -> input         c
    LoadProgram b c   -> loadProgram b c

    Orthography a val -> orthography a val
{-# INLINE runInstruction #-}

condMove :: RegisterNumTriplet -> StateT Machine IO ()
condMove triplet = do
  c <- use $ cReg triplet
  if c == 0
    then return ()
    else aReg triplet <~ use (bReg triplet)

arrayIndex :: RegisterNumTriplet -> StateT Machine IO ()
arrayIndex triplet = do
  b <- use $ bReg triplet
  c <- use $ cReg triplet
  if b > 1000
     then lift . putStrLn $ "arrayIndex, reg info: " ++ show (b, c)
     else return ()
  aReg triplet <~ use (to (getMemElem b (fromIntegral c))) --memoryElement b (fromIntegral c))

arrayAmendment :: RegisterNumTriplet -> StateT Machine IO ()
arrayAmendment triplet = do
  a <- use $ aReg triplet
  b <- use $ bReg triplet
  c <- use $ cReg triplet
  modify $ setMemElem a (fromIntegral b) c
  --memoryElement a (fromIntegral b) <~ use (cReg triplet)

addition :: RegisterNumTriplet -> StateT Machine IO ()
addition triplet = do
  b <- use $ bReg triplet
  c <- use $ cReg triplet
  aReg triplet .= b + c -- fromIntegral ((fromIntegral b + fromIntegral c :: Integer) `rem` (2^(32::Int) :: Integer))

multiplication :: RegisterNumTriplet -> StateT Machine IO ()
multiplication triplet = do
  b <- use $ bReg triplet
  c <- use $ cReg triplet
  aReg triplet .= b * c -- fromIntegral ((fromIntegral b * fromIntegral c :: Integer) `rem` (2^(32::Int) :: Integer))

division :: RegisterNumTriplet -> StateT Machine IO ()
division triplet = do
  b <- use $ bReg triplet
  c <- use $ cReg triplet
  aReg triplet .= (b `quot` c)

notAnd :: RegisterNumTriplet -> StateT Machine IO ()
notAnd triplet = do
  b <- use $ bReg triplet
  c <- use $ cReg triplet
  aReg triplet .= complement (b .&. c)


halt :: StateT Machine IO ()
halt = lift exitSuccess


allocation :: RegisterNum -> RegisterNum -> StateT Machine IO ()
allocation bNum cNum = do
  c      <- use $ register cNum
  arrNum <- newArray c
  register bNum .= arrNum
{-
  unusedArrays <- use unusedMemory
  arrNum <- case unusedArrays of
              []     -> do
                n <- use nextArrayNum
                --liftIO . putStrLn $ "Allocating new array. Array #" ++ show n
                nextArrayNum += 1
                return n
              (n:ns) -> {- liftIO (putStrLn ("Reusing array. Using array #" ++ show n)) >> -} (unusedMemory .= ns) >> return n
  c <- use $ register cNum
  newArray arrNum c
  register bNum .= arrNum
  liftIO performMinorGC
-}

abandonment :: RegisterNum -> StateT Machine IO ()
abandonment cNum = do
  c <- use $ register cNum
  freeArray c
  --liftIO performMajorGC
{-
  c <- use $ register cNum
  unusedMemory %= (c:)
  memory . ix (fromIntegral c) .= VU.empty -- Hopefully free the memory
  liftIO performMajorGC
-}

output :: RegisterNum -> StateT Machine IO ()
output cNum = do
  c <- use $ register cNum
  lift . putChar . chr . fromIntegral $ c

input :: RegisterNum -> StateT Machine IO ()
input cNum = do
  val <- lift $ getChar
  register cNum .= fromIntegral (ord val)

loadProgram :: RegisterNum -> RegisterNum -> StateT Machine IO ()
loadProgram bNum cNum = do
  b <- use $ register bNum
  c <- use $ register cNum
  ip .= c - 1
  copyToZeroArray b
{-
  ip .= c-1
  memory . ix 0 <~ use (memory . ix (fromIntegral b))
-}

orthography :: RegisterNum -> MachineWord -> StateT Machine IO ()
orthography aNum val =
  register aNum .= val
