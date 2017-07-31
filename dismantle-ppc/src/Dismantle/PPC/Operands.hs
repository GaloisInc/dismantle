{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK not-home #-}
module Dismantle.PPC.Operands (
  GPR(..),
  CRBitM(..),
  mkCRBitM,
  crbitmToBits,
  CRBitRC(..),
  CRRC(..),
  FR(..),
  VR(..),
  VSR(..),
  BranchTarget(..),
  mkBranchTarget,
  branchTargetToBits,
  AbsBranchTarget(..),
  mkAbsBranchTarget,
  absBranchTargetToBits,
  MemRI(..),
  mkMemRI,
  memRIToBits,
  MemRIX(..),
  mkMemRIX,
  memRIXToBits,
  MemRR(..),
  mkMemRR,
  memRRToBits,
  SPEDis(..),
  mkSPEDis,
  speDisToBits,
  truncBits,
  signedImmediateToWord32
  ) where

import GHC.TypeLits

import Data.Bits
import Data.Int ( Int16, Int32 )
import Data.Monoid
import Data.Proxy ( Proxy(..) )
import Data.Word ( Word8, Word16, Word32 )

import qualified Text.PrettyPrint.HughesPJClass as PP

import qualified Data.Int.Indexed as I
import Dismantle.Tablegen.TH.Pretty ()
import qualified Dismantle.Arbitrary as A

-- | Condition register field selector, alternate form
--
-- This selects one of the eight 4-bit fields in the condition register. The
-- index is stored here as a normal integer, but is encoded in the instruction
-- as the 8-bit value @0x80 >> idx@, such that exactly the (8 - idx)th bit is
-- set.
newtype CRBitM = CRBitM { unCRBitM :: Word8 }
  deriving (Eq, Ord, Show)

mkCRBitM :: Word32 -> CRBitM
mkCRBitM = CRBitM . fromIntegral . (7 -) . countTrailingZeros

crbitmToBits :: CRBitM -> Word32
crbitmToBits = shiftR 0x80 . fromIntegral . unCRBitM

-- | Condition register bit selector
--
-- This selects a single bit from the 32-bit condition register. It is stored as
-- a normal 5-bit integer.
newtype CRBitRC = CRBitRC { unCRBitRC :: Word8 }
  deriving (Eq, Ord, Show)

-- | Condition register field selector, primary form
--
-- This selects one of the eight 4-bit fields in the condition register. The
-- index is stored as a normal 3-bit integer.
newtype CRRC = CRRC { unCRRC :: Word8 }
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

-- | VSX register by number
newtype VSR = VSR { unVSR :: Word8 }
  deriving (Eq, Ord, Show)

-- | An offset in a branch instruction that is added to the current IP to get
-- the real destination.
--
-- These only actually store 24 bits with (plus two zeros implicitly
-- concatenated on the right)
newtype BranchTarget = BT { unCT :: Int32 }
  deriving (Eq, Ord, Show)

-- | An absolute address of a call target (probably only useful in 32 bit applications)
--
-- These only actually store 24 bits with (plus two zeros implicitly
-- concatenated on the right)
newtype AbsBranchTarget = ABT { unACT :: Word32 }
  deriving (Eq, Ord, Show)

mkBranchTarget :: Word32 -> BranchTarget
mkBranchTarget w = BT (fromIntegral (w `shiftL` 2))

branchTargetToBits :: BranchTarget -> Word32
branchTargetToBits (BT i) = fromIntegral i `shiftR` 2

mkAbsBranchTarget :: Word32 -> AbsBranchTarget
mkAbsBranchTarget w = ABT (w `shiftL` 2)

absBranchTargetToBits :: AbsBranchTarget -> Word32
absBranchTargetToBits (ABT w) = w `shiftR` 2

-- | A vector memory reference for the Signal Processing Engine (SPE) extensions
--
-- The reference is defined as a register reference and a scaled displacement
--
-- The reference is to an Effective Address EA = (RA|0) + (disp*scale)
--
-- The displacement is stored scaled.  It must be a multiple of the type-level
-- nat
data SPEDis (scale :: Nat) = SPEDis (Maybe GPR) Word16
  deriving (Eq, Ord, Show)

mkSPEDis :: forall (s :: Nat) . (KnownNat s) => Word32 -> SPEDis s
mkSPEDis w
  | reg == 0 = SPEDis Nothing (scale * d)
  | otherwise = SPEDis (Just (GPR reg)) (scale * d)
  where
    mask = (1 `shiftL` 5) - 1
    d = fromIntegral (w .&. mask)
    reg = fromIntegral ((w `shiftL` 5) .&. mask)
    scale = fromInteger (natVal (Proxy :: Proxy s))

speDisToBits :: forall (s :: Nat) . (KnownNat s) => SPEDis s -> Word32
speDisToBits spe =
  case spe of
    SPEDis Nothing d -> fromIntegral (d `div` scale)
    SPEDis (Just (GPR reg)) d ->
      fromIntegral (reg `shiftL` 5) .|. fromIntegral (d `div` scale)
  where
    scale = fromInteger (natVal (Proxy :: Proxy s))

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
data MemRI = MemRI (Maybe GPR) Int16
  deriving (Eq, Ord, Show)

mkMemRI :: Word32 -> MemRI
mkMemRI w
  | reg == 0 = MemRI Nothing disp
  | otherwise = MemRI (Just (GPR reg)) disp
  where
    dispMask = (1 `shiftL` 16) - 1
    regMask = (1 `shiftL` 5) - 1
    reg = fromIntegral ((w `shiftR` 16) .&. regMask)
    disp = fromIntegral (w .&. dispMask)

memRIToBits :: MemRI -> Word32
memRIToBits (MemRI mreg disp) =
  case mreg of
    Just (GPR r) -> (fromIntegral r `shiftL` 16) .|. (fromIntegral disp .&. dispMask)
    Nothing -> fromIntegral disp .&. dispMask
  where
    -- When we extend the 'Int16' to a 'Word32', it gets sign extended (which
    -- can set the high bits of the word).  We need to mask the 'Word32' so that
    -- only the actually relevant bits are preserved.
    dispMask = (1 `shiftL` 16) - 1

-- | This operand is just like 'MemRI', but the displacement is concatenated on
-- the right by two zeros
--
-- Note that the low two bits of the Word16 must be 0
data MemRIX = MemRIX (Maybe GPR) Int16
  deriving (Eq, Ord, Show)

mkMemRIX :: Word32 -> MemRIX
mkMemRIX w
  | r == 0 = MemRIX Nothing d
  | otherwise = MemRIX (Just (GPR r)) d
  where
    dispMask = (1 `shiftL` 16) - 1
    regMask = (1 `shiftL` 5) - 1
    r = fromIntegral ((w `shiftR` 16) .&. regMask)
    d = fromIntegral (w .&. dispMask) `shiftL` 2

memRIXToBits :: MemRIX -> Word32
memRIXToBits (MemRIX mr disp) =
  case mr of
    Just (GPR r) -> (fromIntegral r `shiftL` 16) .|. (fromIntegral (disp `shiftR` 2) .&. dispMask)
    Nothing -> fromIntegral (disp `shiftR` 2) .&. dispMask
  where
    dispMask = onesMask 16

-- | Make a mask of @n@ bits set to true
onesMask :: (Bits w, Num w) => Int -> w
onesMask n = (1 `shiftL` n) - 1

signedImmediateToWord32 :: forall (n :: Nat) . (KnownNat n) => I.I n -> Word32
signedImmediateToWord32 i@(I.I w) =
  fromIntegral w .&. onesMask nBits
  where
    nBits = I.width i

truncBits :: (Bits w, Integral w) => Int -> w -> Word32
truncBits nBits w = onesMask nBits .&. fromIntegral w

instance PP.Pretty GPR where
  pPrint (GPR rno) = PP.char 'r' <> PP.int (fromIntegral rno)

instance PP.Pretty CRBitM where
  pPrint (CRBitM rno) = PP.char 'c' <> PP.char 'r' <> PP.int (fromIntegral rno)

instance PP.Pretty CRBitRC where
  pPrint (CRBitRC n) =
    let (crno, kno) = fromIntegral n `divMod` 4
        kstr = case kno of
          0 -> "lt"
          1 -> "gt"
          2 -> "eq"
          3 -> "so"
          _ -> error ("Invalid CRBitRC kind: " ++ show kno)
    in PP.text "4*cr" <> PP.int crno <> PP.char '+' <> PP.text kstr

instance PP.Pretty CRRC where
  pPrint (CRRC rno) = PP.char 'c' <> PP.char 'r' <> PP.int (fromIntegral rno)

instance PP.Pretty FR where
  pPrint (FR rno) = PP.char 'f' <> PP.int (fromIntegral rno)

instance PP.Pretty VR where
  pPrint (VR rno) = PP.char 'v' <> PP.int (fromIntegral rno)

instance PP.Pretty VSR where
  pPrint (VSR rno) = PP.char 'x' <> PP.int (fromIntegral rno)

instance PP.Pretty MemRI where
  pPrint (MemRI mr d) =
    case mr of
      Nothing -> PP.pPrint d
      Just r -> PP.int (fromIntegral d) <> PP.parens (PP.pPrint r)

instance PP.Pretty MemRIX where
  pPrint (MemRIX mr d) =
    case mr of
      Nothing -> PP.pPrint d
      Just r -> PP.int (fromIntegral d) <> PP.parens (PP.pPrint r)

instance PP.Pretty MemRR where
  pPrint (MemRR mra rb) =
    case mra of
      Nothing -> PP.text "0, " <> PP.pPrint rb
      Just ra -> PP.pPrint ra <> PP.text ", " <> PP.pPrint rb

instance (KnownNat s) => PP.Pretty (SPEDis s) where
  pPrint (SPEDis mreg d) =
    case mreg of
      Nothing -> PP.int (fromIntegral d)
      Just r -> PP.int (fromIntegral d) <> PP.parens (PP.pPrint r)

instance PP.Pretty AbsBranchTarget where
  pPrint (ABT w) = PP.pPrint w

instance PP.Pretty BranchTarget where
  pPrint (BT i) = PP.pPrint i

instance A.Arbitrary CRBitM where
  arbitrary g = CRBitM <$> A.uniformR (0, 7) g

instance A.Arbitrary CRBitRC where
  arbitrary g = CRBitRC <$> A.uniformR (0, 31) g

instance A.Arbitrary CRRC where
  arbitrary g = CRRC <$> A.uniformR (0, 7) g

instance A.Arbitrary FR where
  arbitrary g = FR <$> A.uniformR (0, 31) g

instance A.Arbitrary GPR where
  arbitrary g = GPR <$> A.uniformR (0, 31) g

instance A.Arbitrary VR where
  arbitrary g = VR <$> A.uniformR (0, 31) g

instance A.Arbitrary VSR where
  arbitrary g = VSR <$> A.uniformR (0, 63) g

instance A.Arbitrary AbsBranchTarget where
  arbitrary g = ABT <$> A.uniformR (0, maxVal) g
    where
      maxVal = (1 `shiftL` 24) - 1

instance A.Arbitrary BranchTarget where
  arbitrary g = BT <$> A.uniformR (-maxVal, maxVal) g
    where
      maxVal = ((1 `shiftL` 24) - 1) `div` 2

instance A.Arbitrary MemRR where
  arbitrary g = do
    ano <- A.uniformR (0, 31) g
    case ano of
      0 -> MemRR Nothing <$> A.arbitrary g
      _ -> MemRR (Just (GPR ano)) <$> A.arbitrary g

instance A.Arbitrary MemRI where
  arbitrary g = do
    ano <- A.uniformR (0, 31) g
    case ano of
      0 -> MemRI Nothing <$> A.arbitrary g
      _ -> MemRI (Just (GPR ano)) <$> A.arbitrary g

instance A.Arbitrary MemRIX where
  arbitrary g = do
    ano <- A.uniformR (0, 31) g
    case ano of
      0 -> MemRIX Nothing <$> A.arbitrary g
      _ -> MemRIX (Just (GPR ano)) <$> A.arbitrary g

instance A.Arbitrary (SPEDis 2) where
  arbitrary g = do
    ano <- A.uniformR (0, 31) g
    case ano of
      0 -> SPEDis Nothing <$> A.arbitrary g
      _ -> SPEDis (Just (GPR ano)) <$> A.arbitrary g

instance A.Arbitrary (SPEDis 4) where
  arbitrary g = do
    ano <- A.uniformR (0, 31) g
    case ano of
      0 -> SPEDis Nothing <$> A.arbitrary g
      _ -> SPEDis (Just (GPR ano)) <$> A.arbitrary g

instance A.Arbitrary (SPEDis 8) where
  arbitrary g = do
    ano <- A.uniformR (0, 31) g
    case ano of
      0 -> SPEDis Nothing <$> A.arbitrary g
      _ -> SPEDis (Just (GPR ano)) <$> A.arbitrary g
