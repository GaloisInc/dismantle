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
  VSReg(..),
  BranchTarget(..),
  mkBranchTarget,
  branchTargetToBits,
  AbsBranchTarget(..),
  mkAbsBranchTarget,
  absBranchTargetToBits,
  CondBranchTarget(..),
  mkCondBranchTarget,
  condBranchTargetToBits,
  AbsCondBranchTarget(..),
  mkAbsCondBranchTarget,
  absCondBranchTargetToBits,
  MemRI(..),
  mkMemRI,
  memRIToBits,
  MemRIX(..),
  mkMemRIX,
  memRIXToBits,
  MemRR(..),
  mkMemRR,
  memRRToBits,
  truncBits,
  signedImmediateToWord32
  ) where

import           GHC.TypeLits

import           Data.Bits
import           Data.Int ( Int16, Int32 )
import           Data.Monoid
import           Data.Proxy ( Proxy(..) )
import           Data.Word ( Word8, Word16, Word32 )

import qualified Text.PrettyPrint.HughesPJClass as PP

import qualified Data.Int.Indexed as I
import qualified Data.Set.NonEmpty as NES
import           Dismantle.Tablegen.TH.Pretty ()
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
--
-- This is named 'VSReg' instead of @VSR@ because there is also an instruction
-- named @VSR@.
newtype VSReg = VSReg { unVSReg :: Word8 }
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

-- | Truncate the signed value held in a 'Word32' to a 'Int32' of the given number of bits.
--
-- This is done using some shifts to preserve the sign bits.  The high bits of
-- the 'Int32' reflect the sign, even though the underlying value is not allowed
-- to represent an out-of-range value.
--
-- We change the type before the 'shiftR' so that we get sign extension.
truncSigned :: Int -> Word32 -> Int32
truncSigned nBits w =
  shiftR (fromIntegral (shiftL w shiftAmount)) shiftAmount
  where
    shiftAmount = 32 - nBits

mkBranchTarget :: Word32 -> BranchTarget
mkBranchTarget = BT . truncSigned 24

branchTargetToBits :: BranchTarget -> Word32
branchTargetToBits = (.&. 0xffffff) . fromIntegral . unCT

mkAbsBranchTarget :: Word32 -> AbsBranchTarget
mkAbsBranchTarget = ABT . (.&. 0xffffff)

absBranchTargetToBits :: AbsBranchTarget -> Word32
absBranchTargetToBits = (.&. 0xffffff) . unACT

-- | Conditional branch targets are 14 bit signed offsets from the current IP.
newtype CondBranchTarget = CBT { unCBT :: Int32 }
  deriving (Eq, Ord, Show)

newtype AbsCondBranchTarget = ACBT { unACBT :: Word32 }
  deriving (Eq, Ord, Show)

mkCondBranchTarget :: Word32 -> CondBranchTarget
mkCondBranchTarget = CBT . truncSigned 14

condBranchTargetToBits :: CondBranchTarget -> Word32
condBranchTargetToBits = (.&. 0x3fff) . fromIntegral . unCBT

mkAbsCondBranchTarget :: Word32 -> AbsCondBranchTarget
mkAbsCondBranchTarget = ACBT . (.&. 0x3fff)

absCondBranchTargetToBits :: AbsCondBranchTarget -> Word32
absCondBranchTargetToBits = (.&. 0x3fff) . unACBT

-- | Memory addressed by two registers, RA and RB
--
-- NOTE: Sometimes the Effective Address (EA) is (RA|0) + (RB), but other times
-- it is just (RA) + (RB)
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
  | reg == 0 = MemRI Nothing (fromIntegral disp)
  | otherwise = MemRI (Just (GPR reg)) (fromIntegral disp)
  where
    regMask = (1 `shiftL` 5) - 1
    reg = fromIntegral ((w `shiftR` 16) .&. regMask)
    disp = truncSigned 16 w

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
-- the right by two zeros.
data MemRIX = MemRIX (Maybe GPR) (I.I 14)
  deriving (Eq, Ord, Show)

mkMemRIX :: Word32 -> MemRIX
mkMemRIX w
  | r == 0 = MemRIX Nothing (fromIntegral d)
  | otherwise = MemRIX (Just (GPR r)) (fromIntegral d)
  where
    regMask = (1 `shiftL` 5) - 1
    r = fromIntegral ((w `shiftR` 14) .&. regMask)
    d = truncSigned 14 w

memRIXToBits :: MemRIX -> Word32
memRIXToBits (MemRIX mr disp) =
  case mr of
    Just (GPR r) -> (fromIntegral r `shiftL` 14) .|. (fromIntegral (I.unI disp) .&. dispMask)
    Nothing -> fromIntegral (I.unI disp) .&. dispMask
  where
    dispMask = onesMask 14

-- | Make a mask of @n@ bits set to true
onesMask :: (Bits w, Num w) => Int -> w
onesMask n = (1 `shiftL` n) - 1

signedImmediateToWord32 :: forall (n :: Nat) . (KnownNat n) => I.I n -> Word32
signedImmediateToWord32 i@(I.I w) =
  truncBits nBits w
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

instance PP.Pretty VSReg where
  pPrint (VSReg rno) = PP.char 'x' <> PP.int (fromIntegral rno)

instance PP.Pretty MemRI where
  pPrint (MemRI mr d) =
    case mr of
      Nothing -> PP.pPrint d
      Just r -> PP.int (fromIntegral d) <> PP.parens (PP.pPrint r)

instance PP.Pretty MemRIX where
  pPrint (MemRIX mr d) =
    case mr of
      Nothing -> PP.pPrint d
      Just r -> PP.int (fromIntegral (I.unI d) `shiftL` 2) <> PP.parens (PP.pPrint r)

instance PP.Pretty MemRR where
  pPrint (MemRR mra rb) =
    case mra of
      Nothing -> PP.text "0, " <> PP.pPrint rb
      Just ra -> PP.pPrint ra <> PP.text ", " <> PP.pPrint rb

instance PP.Pretty AbsBranchTarget where
  pPrint (ABT w) = PP.pPrint (w `shiftL` 2)

instance PP.Pretty BranchTarget where
  pPrint (BT i) = PP.pPrint (i `shiftL` 2)

instance PP.Pretty AbsCondBranchTarget where
  pPrint (ACBT w) = PP.pPrint (w `shiftL` 2)

instance PP.Pretty CondBranchTarget where
  pPrint (CBT i) = PP.pPrint (i `shiftL` 2)

instance A.Arbitrary CRBitM where
  arbitrary g = CRBitM <$> A.uniformR (0, 7) g

instance A.Arbitrary CRBitRC where
  arbitrary g = CRBitRC <$> A.uniformR (0, 31) g

instance A.Arbitrary CRRC where
  arbitrary g = CRRC <$> A.uniformR (0, 7) g

instance A.Arbitrary FR where
  arbitrary g = FR <$> A.uniformR (0, 31) g

instance A.Arbitrary GPR where
  arbitrary g = GPR <$> A.choose (NES.fromList 0 [3..10]) g

instance A.Arbitrary VR where
  arbitrary g = VR <$> A.uniformR (0, 31) g

instance A.Arbitrary VSReg where
  arbitrary g = VSReg <$> A.uniformR (0, 63) g

instance A.Arbitrary AbsBranchTarget where
  arbitrary g = ABT <$> A.uniformR (0, maxVal) g
    where
      maxVal = (1 `shiftL` 24) - 1

instance A.Arbitrary BranchTarget where
  arbitrary g = BT <$> A.uniformR (-maxVal, maxVal) g
    where
      maxVal = ((1 `shiftL` 24) - 1) `div` 2

instance A.Arbitrary AbsCondBranchTarget where
  arbitrary g = mkAbsCondBranchTarget <$> A.uniformR (-maxVal, maxVal) g
    where
      maxVal = ((1 `shiftL` 14) - 1)

instance A.Arbitrary CondBranchTarget where
  arbitrary g = mkCondBranchTarget <$> A.uniformR (-maxVal, maxVal) g
    where
      maxVal = ((1 `shiftL` 14) - 1) `div` 2

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

{- Note [Truncation]

We have many oddly-sized fields in operands.  As part of construction, we want
to throw away any excess bits that would not fit in the field (since the backing
store can actually hold and return them - if we didn't, we might get
out-of-range values when we go to re-construct an operand).

Note that in many cases, a signed value of a few bits (e.g., 3-24 or so) are
stored in a Word32.  In an earlier version of this code, we would use a mask to
extract only the number of bits we cared about.  This was problematic, as it
would chop off sign bits and cause some very unfortunate errors.  The new
approach at operand construction time involves shifts, which perform the correct
sign extension as necessary.

We still extract values from operands using masking, as we really want to get
rid of excess sign bits.  During extraction, we are building up a Word32 that
will be ORed into an instruction.

-}
