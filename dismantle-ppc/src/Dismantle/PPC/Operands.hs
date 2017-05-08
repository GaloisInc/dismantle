{-# OPTIONS_HADDOCK not-home #-}
module Dismantle.PPC.Operands (
  GPR(..),
  CR(..),
  FR(..),
  VR(..),
  MemRI(..),
  mkMemRI,
  memRIToBits,
  MemRIX(..),
  mkMemRIX,
  memRIXToBits,
  MemRR(..),
  mkMemRR,
  memRRToBits
  ) where

import Data.Bits
import Data.Monoid
import Data.Word ( Word8, Word16, Word32 )

import qualified Text.PrettyPrint.HughesPJClass as PP

newtype CR = CR { unCR :: Word8 }
  deriving (Eq, Ord, Show)

-- | Floating-point register by number
newtype FR = FR { unFR :: Word8 }
  deriving (Eq, Ord, Show)

-- | General-purpose register by number
newtype GPR = GPR { unGPR :: Word8 }
  deriving (Eq, Ord, Show)

-- | Vector register by number
newtype VR = VR { unVR :: Word8 }
  deriving (Eq, Ord, Show)

-- | Memory addressed by two registers, RA and RB
--
-- The Effective Address (EA) is (RA|0) + (RB)
data MemRR = MemRR (Maybe GPR) GPR
  deriving (Eq, Ord, Show)

mkMemRR :: Word32 -> MemRR
mkMemRR w
  | ra == 0 = MemRR Nothing (GPR rb)
  | otherwise = MemRR (Just (GPR ra)) (GPR rb)
  where
    regMask = (1 `shiftL` 5) - 1
    ra = fromIntegral ((w `shiftR` 5) .&. regMask)
    rb = fromIntegral (w .&. regMask)

memRRToBits :: MemRR -> Word32
memRRToBits m =
  case m of
    MemRR Nothing (GPR rb) -> fromIntegral rb
    MemRR (Just (GPR ra)) (GPR rb) -> (fromIntegral ra `shiftL` 5) .|. fromIntegral rb

-- | A memory reference defined by a register and immediate offset
--
-- The reference is an address stored in a general-purpose register
-- plus an optional constant displacement.  The low 16 bits are the
-- displacement, while the top 5 bits are the register reference.
data MemRI = MemRI GPR Word16
  deriving (Eq, Ord, Show)

mkMemRI :: Word32 -> MemRI
mkMemRI w = MemRI (GPR (fromIntegral ((w `shiftR` 16) .&. regMask))) (fromIntegral (w .&. dispMask))
  where
    dispMask = (1 `shiftL` 16) - 1
    regMask = (1 `shiftL` 5) - 1

memRIToBits :: MemRI -> Word32
memRIToBits (MemRI (GPR r) disp) =
  (fromIntegral r `shiftL` 16) .|. fromIntegral disp

-- | This operand is just like 'MemRI', but the displacement is concatenated on
-- the right by two zeros
--
-- FIXME: Make GRP a Maybe, in the case the register is 0
--
-- Note that the low two bits of the Word16 must be 0
data MemRIX = MemRIX GPR Word16
  deriving (Eq, Ord, Show)

mkMemRIX :: Word32 -> MemRIX
mkMemRIX w = MemRIX (GPR r) d
  where
    dispMask = (1 `shiftL` 16) - 1
    regMask = (1 `shiftL` 5) - 1
    r = fromIntegral ((w `shiftR` 16) .&. regMask)
    d = fromIntegral (w .&. dispMask) `shiftL` 2

memRIXToBits :: MemRIX -> Word32
memRIXToBits (MemRIX (GPR r) disp) =
  (fromIntegral r `shiftL` 16) .|. fromIntegral (disp `shiftR` 2)

instance PP.Pretty GPR where
  pPrint (GPR rno) = PP.char 'r' <> PP.int (fromIntegral rno)

instance PP.Pretty CR where
  pPrint (CR rno) = PP.char 'c' <> PP.int (fromIntegral rno)

instance PP.Pretty FR where
  pPrint (FR rno) = PP.char 'f' <> PP.int (fromIntegral rno)

instance PP.Pretty VR where
  pPrint (VR rno) = PP.char 'v' <> PP.int (fromIntegral rno)

instance PP.Pretty MemRI where
  pPrint (MemRI r d)
    | d == 0 = PP.parens (PP.pPrint r)
    | otherwise = PP.int (fromIntegral d) <> PP.parens (PP.pPrint r)

instance PP.Pretty MemRIX where
  pPrint (MemRIX r d)
    | d == 0 = PP.parens (PP.pPrint r)
    | otherwise = PP.int (fromIntegral d) <> PP.parens (PP.pPrint r)

instance PP.Pretty MemRR where
  pPrint (MemRR mra rb) =
    case mra of
      Nothing -> PP.pPrint rb
      Just ra -> PP.pPrint ra <> PP.text ", " <> PP.pPrint rb

