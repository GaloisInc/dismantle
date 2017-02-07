{-# OPTIONS_HADDOCK not-home #-}
module Dismantle.PPC.Operands (
  GPR(..),
  CR(..),
  FR(..),
  VR(..),
  Mem(..),
  mkMem
  ) where

import Data.Bits
import Data.Monoid
import Data.Word ( Word8, Word16, Word32 )

import qualified Text.PrettyPrint.HughesPJClass as PP

newtype CR = CR Word8
  deriving (Eq, Ord, Show)

newtype FR = FR Word8
  deriving (Eq, Ord, Show)

newtype GPR = GPR Word8
  deriving (Eq, Ord, Show)

newtype VR = VR Word8
  deriving (Eq, Ord, Show)

mkMem :: Word32 -> Mem
mkMem w = Mem (GPR (fromIntegral ((w `shiftR` 16) .&. regMask))) (fromIntegral (w .&. dispMask))
  where
    dispMask = (1 `shiftL` 16) - 1
    regMask = (1 `shiftL` 5) - 1

-- | A memory reference for a load or store instruction
--
-- The reference is an address stored in a general-purpose register
-- plus an optional constant displacement.  The low 16 bits are the
-- displacement, while the top 5 bits are the register reference.
data Mem = Mem GPR Word16
  deriving (Eq, Ord, Show)

instance PP.Pretty GPR where
  pPrint (GPR rno) = PP.char 'r' <> PP.int (fromIntegral rno)

instance PP.Pretty CR where
  pPrint (CR rno) = PP.char 'c' <> PP.int (fromIntegral rno)

instance PP.Pretty FR where
  pPrint (FR rno) = PP.char 'f' <> PP.int (fromIntegral rno)

instance PP.Pretty VR where
  pPrint (VR rno) = PP.char 'v' <> PP.int (fromIntegral rno)

instance PP.Pretty Mem where
  pPrint (Mem r d)
    | d == 0 = PP.parens (PP.pPrint r)
    | otherwise = PP.int (fromIntegral d) <> PP.parens (PP.pPrint r)
