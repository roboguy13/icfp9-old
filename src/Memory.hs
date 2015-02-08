{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts, BangPatterns, MultiWayIf #-}
module Memory
  (MachineWord
  ,Memory
  ,Register
  ,RegisterNum

  ,Machine
  ,loadMachineProgram

  ,RegisterLens

  ,memory
  ,ip
  ,registers
  ,unusedMemory
  ,nextArrayNum

  ,newArray
  ,freeArray
  ,copyToZeroArray

  ,register
  ,setMemElem
  ,getMemElem
--  ,memoryElement
  )
  where

import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.Heap (MaxHeap)
import qualified Data.Heap as H
import           Data.Word

import           Data.Monoid
import           Data.List (find)

import           Control.Lens
import           Control.Monad.State.Strict

import Debug.Trace


type MachineWord = Word32
type Memory      = Seq MachineWord
data MemoryInfo  =
  MemoryInfo
  { _arraySize  :: MachineWord
  , _arrayIndex :: Int
  , _arrayNum   :: MachineWord
  }
  deriving (Eq, Show)
makeLenses ''MemoryInfo

type Register    = Word32
type RegisterNum = Word8
data Registers   = Registers
  { _r1 :: !Register, _r2 :: !Register, _r3 :: !Register, _r4 :: !Register
  , _r5 :: !Register, _r6 :: !Register, _r7 :: !Register, _r8 :: !Register
  } deriving Show
makeLenses ''Registers

instance Ord MemoryInfo where
  compare (MemoryInfo a _ _) (MemoryInfo b _ _) = compare a b

data Machine =
  Machine
  { _zeroArray    :: Memory
  , _memory       :: Memory
  , _arrayInfo    :: IntMap MemoryInfo
  , _ip           :: !MachineWord
  , _registers    :: Registers
  , _unusedMemory :: MaxHeap MemoryInfo
  , _nextArrayNum :: !MachineWord
  }

makeLenses ''Machine

initialMachine :: Machine
initialMachine =
  Machine
    { _zeroArray    = mempty
    , _memory       = mempty
    , _arrayInfo    = mempty
    , _ip           = 0
    , _registers    = Registers 0 0 0 0 0 0 0 0
    , _unusedMemory = mempty
    , _nextArrayNum = 1
    }

loadMachineProgram :: [MachineWord] -> Machine
loadMachineProgram program =
--  trace ("programLength: " ++ show (length program))
  initialMachine & zeroArray .~ S.fromList program
{-# INLINE loadMachineProgram #-}

type RegisterLens  = Lens' Machine Register

register :: Word8 -> Lens' Machine Register
register n
  = {-# SCC "register" #-}
   if | n == 0 -> registers . r1
      | n == 1 -> registers . r2
      | n == 2 -> registers . r3
      | n == 3 -> registers . r4
      | n == 4 -> registers . r5
      | n == 5 -> registers . r6
      | n == 6 -> registers . r7
      | n == 7 -> registers . r8
      | otherwise -> error $ "Invalid register number: " ++ show n

{-
register n = {-# SCC "register" #-}
  case n of
    0 -> registers . r1
    1 -> registers . r2
    2 -> registers . r3
    3 -> registers . r4
    4 -> registers . r5
    5 -> registers . r6
    6 -> registers . r7
    7 -> registers . r8
    _ -> error $ "Invalid register number: " ++ show n
-}
{-# INLINE register #-}


-- | We might need to subtract 1 to compensate for using zeroArray
realArrNum :: (Integral a, Num b) => a -> b
realArrNum arrNum = fromIntegral $ arrNum - 1

newArray :: (MonadIO m, MonadState Machine m) => MachineWord -> m Register
newArray size = do
  arrNum <- newArray' size
  (if arrNum > 200
      then trace $ "newArray: arrNum = " ++ show arrNum
      else id) $ return arrNum

newArray' :: (MonadIO m, MonadState Machine m) => MachineWord -> m Register
newArray' size = {-# SCC "newArray" #-} do
  x <- use $ unusedMemory . to H.view
  case x of
    Nothing                       -> wholeNewArray   size
    Just (info, unusedMem)
      | info ^. arraySize >= size -> let arrNum = info ^. arrayNum
                                     in do
                                       reuseArray    arrNum size
                                       unusedMemory .= unusedMem
                                       return arrNum
      | otherwise                 -> wholeNewArray   size
{-
case {-# SCC "newArray.find" #-} find (\i -> (machine ^. arrayInfo . to (imLookup (fromIntegral i)) . arraySize) >= size) $ machine ^. unusedMemory of
    Nothing -> wholeNewArray   size
    Just i  -> reuseArray    i size >> return i
-}
{-# INLINE newArray #-}

wholeNewArray :: (MonadIO m, MonadState Machine m) => MachineWord -> m Register
wholeNewArray size = do
  arrNum <- use nextArrayNum
  nextArrayNum += 1

  i <- use $ memory . to S.length

  memory     <>= S.replicate (fromIntegral size) 0
  arrayInfo   %= IM.insert (realArrNum arrNum) (MemoryInfo { _arraySize = size, _arrayIndex = fromIntegral i, _arrayNum = arrNum })
  --liftIO $ putStrLn $ "wholeNewArray: insert #" ++ show (realArrNum arrNum)
  return arrNum
{-# INLINE wholeNewArray #-}

reuseArray :: (MonadIO m, MonadState Machine m) => Register -> MachineWord -> m ()
reuseArray arrNum size = {-# SCC "reuseArray" #-} do
  im <- use arrayInfo
  --liftIO $ putStrLn $ "reuseArray #" ++ show arrNum ++ ", im = " ++ show im
  i <- use $ arrayInfo . to (imLookup (realArrNum arrNum)) . arrayIndex
  arrayInfo   %= IM.insert (realArrNum arrNum) (MemoryInfo { _arraySize = size, _arrayIndex = i, _arrayNum = arrNum })
  --liftIO $ putStrLn $ "reuseArray: insert #" ++ show (realArrNum arrNum)

{-# INLINE reuseArray #-}


freeArray :: (MonadIO m, MonadState Machine m) => Register -> m ()
freeArray 0      = return ()
freeArray arrNum = do
  arrInfo <- use $ arrayInfo . to (imLookup (realArrNum arrNum))
  --arrayInfo    %= IM.adjust (arraySize .~ 0) (realArrNum arrNum)
  unusedMemory %= H.insert arrInfo
{-# INLINE freeArray #-}

copyToZeroArray :: (MonadIO m, MonadState Machine m) => Register -> m ()
copyToZeroArray 0      = return ()
copyToZeroArray arrNum = do
  arrSize <- use $ arrayInfo . to (imLookup (realArrNum arrNum)) . arraySize
  machine <- get
  zeroArray .= (fmap (\elemNum -> getMemElem arrNum elemNum machine {- (elemNum, machine) ^. memoryElement arrNum -}) (S.fromList [0..fromIntegral arrSize-1]))
{-# INLINE copyToZeroArray #-}

imLookup :: Show a => Int -> IntMap a -> a
imLookup i !im = {-# SCC "imLookup" #-}
  case IM.lookup i im of
    Nothing -> error $ "Internal error: Cannot find index " ++ show i ++ ", im = " ++ show im
    Just x  -> x
{-# INLINE imLookup #-}

realIx :: Machine -> Register -> Int -> Int
realIx _       0      elemNum  = elemNum        -- This branch shouldn't get used
realIx machine arrNum elemNum  = {-# SCC "realIx" #-}
--  (if arrNum > 600 then trace ("realIx: arrayInfo = " ++ show (machine ^. arrayInfo)) else id) $
  case IM.lookup (realArrNum arrNum) (machine ^. arrayInfo) of
    Nothing -> error $ "Internal error: Couldn't find arrayInfo for array #" ++ show arrNum ++ " (realArrNum = " ++ show (realArrNum arrNum) ++ ", element #" ++ show elemNum ++ ")\nmachine: " ++ show (machine ^. registers) ++ "\nip = " ++ show (machine ^. ip)
    Just x  -> traceShow (x, arrNum, elemNum) $ (x ^. arrayIndex) + elemNum
{-# INLINE realIx #-}


{-
memoryElement :: Register -> Lens' (Int, Machine) MachineWord
memoryElement 0 = {-# SCC "memoryElement-0" #-} lens getter setter
  where
    getter (elemNum, machine)   = S.index (machine ^. zeroArray) (fromIntegral elemNum)
    setter (elemNum, machine) x = (elemNum, machine & zeroArray %~ S.update (fromIntegral elemNum) x)

memoryElement arrNum = {-# SCC "memoryElement" #-} {- (if arrNum > 600 then trace ("memoryElement: arrNum = " ++ show arrNum ++ ", elemNum = " ++ show elemNum) else id) $ -} lens getter setter
  where
    getter (elemNum, machine)   = S.index (machine ^. memory)  (fromIntegral $ realIx machine arrNum elemNum)
    setter (elemNum, machine) x = (elemNum, machine & memory %~ S.update (fromIntegral $ realIx machine arrNum elemNum) x)
{-# INLINE memoryElement #-}
-}

setMemElem :: Register -> Int -> MachineWord -> Machine -> Machine
setMemElem 0      elemNum x machine = machine & zeroArray %~ S.update (fromIntegral elemNum) x
setMemElem arrNum elemNum x machine = machine & memory %~ S.update (fromIntegral $ realIx machine arrNum elemNum) x

getMemElem :: Register -> Int -> Machine -> MachineWord
getMemElem 0      elemNum machine  = S.index (machine ^. zeroArray) (fromIntegral elemNum)
getMemElem arrNum elemNum machine  = S.index (machine ^. memory) (fromIntegral $ realIx machine arrNum elemNum)



{-
where
    getter :: Machine -> MachineWord
    getter machine =
      VU.unsafeIndex ((machine ^. memory) IM.! fromIntegral arrNum) (fromIntegral elemNum)
      --case machine ^? memory . ix (fromIntegral arrNum) . ix (fromIntegral elemNum) of
        --Nothing -> error $ "Invalid index: Array #" ++ show arrNum ++ ", Element #" ++ show elemNum ++ "\nArray = " ++ show (machine ^? memory . ix (fromIntegral arrNum))
        --Just e  -> e
    setter :: Machine -> MachineWord -> Machine
    setter machine val =
      machine & memory . ix (fromIntegral arrNum) . ix (fromIntegral elemNum) .~ val
-}
