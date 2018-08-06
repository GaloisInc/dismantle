{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Dismantle.ARM.Operands (
  GPR,
  gpr,
  unGPR,

  DPR,
  dpr,
  unDPR,

  QPR,
  qpr,
  unQPR,

  QQPR,
  qqpr,
  unQQPR,

  Reglist,
  mkRegList,
  regListToBits,

  Bit,
  mkBit,
  bitToBits,

  SBit,
  mkSBit,
  sBitToBits,

  CoprocRegister,
  mkCoprocRegister,
  coprocRegisterToBits,

  Opcode15,
  mkOpcode15,
  opcode15ToBits,

  Opcode7,
  mkOpcode7,
  opcode7ToBits,

  ShiftImm(..),
  mkShiftImm,
  shiftImmToBits,

  Imm8S4(..),
  mkImm8s4,
  imm8s4ToBits,

  RegWithAdd(..),
  mkRegWithAdd,
  regWithAddToBits,

  SvcOperand,
  mkSvcOperand,
  svcOperandToBits,

  BankedReg,
  mkBankedReg,
  bankedRegToBits,

  AddrMode3(..),
  mkAddrMode3,
  addrMode3ToBits,

  AM3Offset(..),
  mkAM3Offset,
  am3OffsetToBits,

  Am2OffsetReg(..),
  mkAm2OffsetReg,
  am2OffsetRegToBits,

  Am2OffsetImm(..),
  mkAm2OffsetImm,
  am2OffsetImmToBits,

  AddrMode5(..),
  mkAddrMode5,
  addrMode5ToBits,

  LdstSoReg(..),
  mkLdstSoSreg,
  ldstSoRegToBits,

  AddrModeImm12(..),
  mkAddrModeImm12,
  addrModeImm12ToBits,

  BranchTarget,
  mkBranchTarget,
  branchTargetToBits,

  BranchExecuteTarget,
  mkBranchExecuteTarget,
  branchExecuteTargetToBits,

  ModImm(..),
  mkModImm,
  modImmToBits,

  Imm5,
  mkImm5,
  imm5ToBits,

  Imm16,
  mkImm16,
  imm16ToBits,

  AdrLabel(..),
  mkAdrLabel,
  adrLabelToBits,

  Pred,
  mkPred,
  predToBits,

  MSRMask(..),
  mkMSRMask,
  msrMaskToBits,

  SoRegImm(..),
  mkSoRegImm,
  soRegImmToBits,

  SoRegReg(..),
  mkSoRegReg,
  soRegRegToBits,

  ShiftImmSpec(..),

  MemBarrierOpt(..),
  mkMemBarrierOpt,
  memBarrierOptToBits
  ) where

import GHC.TypeLits

import Data.Bits
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Word ( Word8, Word32 )
import Data.Int ( Int16, Int32 )
import qualified Data.Word.Indexed as W
import qualified Data.Parameterized.NatRepr as NR

import Numeric (showHex)

import qualified Text.PrettyPrint.HughesPJClass as PP

import qualified Dismantle.Arbitrary as A
import Dismantle.Tablegen.TH.Pretty

-- | A bit field description and functions for using it

data Field n where
    Field :: (KnownNat n)
          => { fieldBits :: NR.NatRepr n
             -- ^ The number of bits in the field
             , fieldOffset :: Int
             -- ^ The offset of the rightmost bit in the field,
             -- starting from zero
             } -> Field n

field :: (KnownNat n) => Int -> Field n
field off = Field { fieldBits = NR.knownNat
                  , fieldOffset = off
                  }

mkMask :: Field n -> Word32
mkMask (Field sz offset) = (2 ^ (NR.widthVal sz) - 1) `shiftL` offset

insert :: Field n -> W.W n -> Word32 -> Word32
insert (Field sz offset) src dest =
    dest .|. (((fromIntegral $ W.unW src) .&. (2 ^ (NR.widthVal sz) - 1)) `shiftL` offset)

extract :: (KnownNat n) => Field n -> Word32 -> W.W n
extract f val = W.w $ fromIntegral $ (val .&. (mkMask f)) `shiftR` (fieldOffset f)

-- Operand data types

newtype Bit = Bit { unBit :: W.W 1 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Bit where
  pPrint (Bit v) = PP.int $ fromIntegral $ W.unW v

mkBit :: Word32 -> Bit
mkBit v = Bit $ W.w $ fromIntegral v

bitToBits :: Bit -> Word32
bitToBits (Bit v) = fromIntegral $ W.unW v

newtype SBit = SBit { unSBit :: W.W 1 }
  deriving (Eq, Ord, Show)

instance PP.Pretty SBit where
  pPrint (SBit 1) = PP.text "s"
  pPrint (SBit 0) = PP.text ""
  pPrint (SBit v) = error $ "Invalid SBit value: " <> show v

mkSBit :: Word32 -> SBit
mkSBit v = SBit $ W.w $ fromIntegral v

sBitToBits :: SBit -> Word32
sBitToBits (SBit v) = fromIntegral $ W.unW v

data Reglist = Reglist { unReglist :: W.W 16 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Reglist where
  pPrint (Reglist bits) =
      let positions :: [Int]
          positions = [0..15]
          members = catMaybes $ regMember <$> positions
          regMember rn = if (((bits `shiftR` rn) .&. 0b1) == 0b1)
                         then Just rn
                         else Nothing
          regs = GPR <$> fromIntegral <$> members
      in PP.braces (PP.hsep $ PP.punctuate (PP.text ",") (PP.pPrint <$> regs))

mkRegList :: Word32 -> Reglist
mkRegList v = Reglist $ W.w $ fromIntegral v

regListToBits :: Reglist -> Word32
regListToBits (Reglist v) = fromIntegral $ W.unW v

-- | General-purpose register by number
newtype GPR = GPR { unGPR :: W.W 4 }
  deriving (Eq, Ord, Show)

instance PP.Pretty GPR where
  pPrint (GPR 10) = PP.text "sl"
  pPrint (GPR 11) = PP.text "fp"
  pPrint (GPR 12) = PP.text "ip"
  pPrint (GPR 13) = PP.text "sp"
  pPrint (GPR 14) = PP.text "lr"
  pPrint (GPR 15) = PP.text "pc"
  pPrint (GPR rno) = PP.char 'r' <> PP.int (fromIntegral $ W.unW rno)

gpr :: Word32 -> GPR
gpr = GPR . W.w . fromIntegral

-- | Coprocessor register by number
newtype CoprocRegister = CoprocRegister { unCoprocRegister :: W.W 4 }
  deriving (Eq, Ord, Show)

mkCoprocRegister :: Word32 -> CoprocRegister
mkCoprocRegister w = CoprocRegister $ W.w $ fromIntegral w

coprocRegisterToBits :: CoprocRegister -> Word32
coprocRegisterToBits (CoprocRegister i) = fromIntegral $ W.unW i

instance PP.Pretty CoprocRegister where
  pPrint (CoprocRegister r) = PP.text "cr" <> PP.pPrint r

-- | Coprocessor operation 15-bit opcode register by number
newtype Opcode15 = Opcode15 { unOpcode15 :: W.W 4 }
  deriving (Eq, Ord, Show)

mkOpcode15 :: Word32 -> Opcode15
mkOpcode15 w = Opcode15 $ fromIntegral w

opcode15ToBits :: Opcode15 -> Word32
opcode15ToBits (Opcode15 i) = fromIntegral $ W.unW i

instance PP.Pretty Opcode15 where
  pPrint (Opcode15 o) = PP.pPrint o

-- | Coprocessor operation 7-bit opcode register by number
newtype Opcode7 = Opcode7 { unOpcode7 :: W.W 3 }
  deriving (Eq, Ord, Show)

mkOpcode7 :: Word32 -> Opcode7
mkOpcode7 w = Opcode7 $ fromIntegral w

opcode7ToBits :: Opcode7 -> Word32
opcode7ToBits (Opcode7 i) = fromIntegral $ W.unW i

instance PP.Pretty Opcode7 where
  pPrint (Opcode7 o) = PP.pPrint o

-- | Double-precision register by number
newtype DPR = DPR { unDPR :: W.W 5 }
  deriving (Eq, Ord, Show)

dpr :: Word8 -> DPR
dpr = DPR . W.w . fromIntegral

instance PP.Pretty DPR where
  pPrint (DPR rno) = PP.char 'd' <> PP.int (fromIntegral $ W.unW rno)

-- | 128-bit vector register by number
newtype QPR = QPR { unQPR :: W.W 5 }
  deriving (Eq, Ord, Show)

qpr :: Word32 -> QPR
qpr = QPR . W.w . fromIntegral

instance PP.Pretty QPR where
  pPrint (QPR rno) = PP.char 'q' <> PP.int (fromIntegral $ W.unW rno)

-- | 256-bit vector register (128-bit register pair) by number (must be
-- even)
newtype QQPR = QQPR { unQQPR :: W.W 4 }
  deriving (Eq, Ord, Show)

qqpr :: Word32 -> QQPR
qqpr = QQPR . W.w . fromIntegral

instance PP.Pretty QQPR where
  pPrint (QQPR rno) = PP.char 'q' <> PP.int (fromIntegral $ W.unW rno)

-- | An AddrMode_Imm12 memory reference for a load or store instruction
-- (with a 12-bit immediate)
data AddrModeImm12 = AddrModeImm12 { addrModeImm12Register  :: GPR
                                   , addrModeImm12Immediate :: W.W 12
                                   , addrModeImm12Add       :: W.W 1
                                   }
  deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeImm12 where
  pPrint m =
      let s = PP.text $ if addrModeImm12Add m == 0 then "-" else ""
          suf = if addrModeImm12Immediate m == 0
                then mempty
                else PP.char ',' PP.<+> ((PP.char '#') <> (s <> (PP.pPrint $ addrModeImm12Immediate m)))
      in PP.brackets $ PP.pPrint (addrModeImm12Register m) <> suf

addrModeImm12RegField :: Field 4
addrModeImm12RegField = field 13

addrModeImm12AddField :: Field 1
addrModeImm12AddField = field 12

addrModeImm12ImmField :: Field 12
addrModeImm12ImmField = field 0

mkAddrModeImm12 :: Word32 -> AddrModeImm12
mkAddrModeImm12 w = AddrModeImm12 (GPR reg) imm add
  where
    reg = extract addrModeImm12RegField w
    add = extract addrModeImm12AddField w
    imm = extract addrModeImm12ImmField w

addrModeImm12ToBits :: AddrModeImm12 -> Word32
addrModeImm12ToBits (AddrModeImm12 (GPR r) imm a) =
    insert addrModeImm12RegField r $
    insert addrModeImm12AddField a $
    insert addrModeImm12ImmField imm 0

-- | An AddrMode3 memory reference for a load or store instruction
data AddrMode3 = AddrMode3 { addrMode3Register  :: GPR
                           , addrMode3Immediate :: W.W 8
                           , addrMode3Type      :: W.W 1
                           , addrMode3Add       :: W.W 1
                           }
  deriving (Eq, Ord, Show)

instance PP.Pretty AddrMode3 where
  pPrint m =
      case addrMode3Type m of
          0 ->
              -- Interpret the low four bits of the immediate field as a
              -- register number (see bit 21 of LDRH variants, or bit 13
              -- of this operand)
              let r2 = GPR $ 0xf .&. (fromIntegral $ W.unW $ addrMode3Immediate m)
                  s = if addrMode3Add m == 0 then "-" else ""
              in PP.brackets $
                 PP.pPrint (addrMode3Register m) <> (PP.char ',' PP.<+> (PP.text s <> PP.pPrint r2))
          1 ->
              -- Interpet all bits of the immediate as an immediate
              let addImm = if addrMode3Immediate m == 0
                           then id
                           else (<> (PP.char ',' PP.<+> (PP.char '#' <> PP.text s <> (PP.pPrint (addrMode3Immediate m)))))
                  s = if addrMode3Add m == 0 then "-" else ""
              in PP.brackets $ addImm $ PP.pPrint (addrMode3Register m)
          v -> error $ "Invalid type value for AddrMode3: " <> show v

addrMode3RegField :: Field 4
addrMode3RegField = field 9

addrMode3AddField :: Field 1
addrMode3AddField = field 8

addrMode3TypeField :: Field 1
addrMode3TypeField = field 13

addrMode3ImmField :: Field 8
addrMode3ImmField = field 0

mkAddrMode3 :: Word32 -> AddrMode3
mkAddrMode3 w = AddrMode3 (GPR reg) imm ty add
  where
    reg = extract addrMode3RegField w
    add = extract addrMode3AddField w
    imm = extract addrMode3ImmField w
    ty  = extract addrMode3TypeField w

addrMode3ToBits :: AddrMode3 -> Word32
addrMode3ToBits (AddrMode3 (GPR r) imm ty add) =
    insert addrMode3RegField r $
    insert addrMode3AddField add $
    insert addrMode3ImmField imm $
    insert addrMode3TypeField ty 0

-- | An am3Offset memory reference for a load or store instruction
data AM3Offset = AM3Offset { am3OffsetImmediate :: W.W 8
                           , am3OffsetOther     :: W.W 1
                           , am3OffsetAdd       :: W.W 1
                           }
  deriving (Eq, Ord, Show)

instance PP.Pretty AM3Offset where
  pPrint m =
      let r = GPR $ 0xf .&. (fromIntegral $ W.unW i)
          i = am3OffsetImmediate m
      in (if am3OffsetAdd m == 0 then PP.char '-' else mempty) <> PP.pPrint r

am3OffsetAddField :: Field 1
am3OffsetAddField = field 8

am3OffsetOtherField :: Field 1
am3OffsetOtherField = field 9

am3OffsetImmField :: Field 8
am3OffsetImmField = field 0

mkAM3Offset :: Word32 -> AM3Offset
mkAM3Offset w = AM3Offset imm other add
  where
    add = extract am3OffsetAddField w
    imm = extract am3OffsetImmField w
    other = extract am3OffsetOtherField w

am3OffsetToBits :: AM3Offset -> Word32
am3OffsetToBits (AM3Offset imm other add) =
    insert am3OffsetAddField add $
    insert am3OffsetOtherField other $
    insert am3OffsetImmField imm 0

-- Notes regarding <shift> as documented in F2.4.1, pg F2-2419,
-- "Constant shifts", and represented by the DecodeImmShift pseudocode
-- in F2.4.3, page F4-2420.  There are 4 main shifts (LSL, LSR, ASR,
-- and ROR) and therefore these are represented by a 2-bit "ty" field
-- in the opcode (in the above value order).
---
-- The shifts are accompanied by a shift amount, which is typically a
-- 5-bit immediate value (thus representing values of 0-31).  Most
-- shifts are in the range of 1-32, where an imm value of 0 indicates
-- 32, with 2 exceptions noted below:
--
-- If the ty is LSL and the value is 0, then no shift is performed
-- (instead of shifting left by 32).
--
-- If the ty is ROR and the value is 0, then this represents a special
-- shift called "RRX" (rotate right 1 bit with extend.
--
-- To support this, there is an abstract type implemented which can be
-- constructed with the core values and which has an instance for
-- pretty-printing.

data ShiftImmSpec = ShiftImmSpec { si_ty :: W.W 2, si_amt :: W.W 5 } deriving Eq

instance PP.Pretty ShiftImmSpec where
    pPrint s =
        let i = si_amt s
            ppAmt = PP.text $ "#" <> show (if i == 0 then 32 else W.unW i)
        in case si_ty s of
             0b00 -> if i == 0
                     then mempty
                     else PP.comma <> PP.space <> (PP.text "lsl" PP.<+> ppAmt)
             0b01 -> PP.comma <> PP.space <> (PP.text "lsr" PP.<+> ppAmt)
             0b10 -> PP.comma <> PP.space <> (PP.text "asr" PP.<+> ppAmt)
             0b11 -> PP.comma <> PP.space <> (if i == 0
                                              then PP.text "rrx"
                                              else PP.text "ror" PP.<+> ppAmt)

-- Shifting from register values is different, and should use
-- ShiftRegSpec instead of ShiftImmSpec.  (to match pseudocode
-- DecodeRegShift).  The ShiftRegSpec only recognizes the 4 main
-- shifts.  The actual shift operation use the Shift_C pseudocode,
-- which implies support for RRX but the shift amount (retrieved from
-- a register) is used unchanged (e.g. 0-31 for all shifts).

data ShiftRegSpec = ShiftRegSpec { sr_ty :: W.W 2, sr_amt :: GPR } deriving Eq

instance PP.Pretty ShiftRegSpec where
    pPrint s =
        let ppShiftType = case sr_ty s of
              0b00 -> PP.text "lsl"
              0b01 -> PP.text "lsr"
              0b10 -> PP.text "asr"
              0b11 -> PP.text "ror"
        in ppShiftType PP.<+> (PP.pPrint $ sr_amt s)


data Imm8S4 = Imm8S4 { imm8s4Add :: W.W 1
                     , imm8s4Immediate :: W.W 8
                     }
    deriving (Eq, Ord, Show)

instance PP.Pretty Imm8S4 where
    pPrint (Imm8S4 a i) =
        let s = PP.text $ if a == 1 then "" else "-"
        in PP.char '#' <> s <> PP.pPrint (((fromIntegral $ W.unW i) :: Word32) `shiftL` 2)

mkImm8s4 :: Word32 -> Imm8S4
mkImm8s4 w = Imm8S4 add imm8
  where
    add = extract (field 8) w
    imm8 = extract (field 0) w

imm8s4ToBits :: Imm8S4 -> Word32
imm8s4ToBits (Imm8S4 add imm) =
    insert (field 8) add $
    insert (field 0) imm 0

-- | A shift_imm operand with a shift immediate and shift type (l/r).
-- See also USAT in the ARM ARM and tgen.
data ShiftImm = ShiftImm { shiftImmImmediate :: W.W 5
                         , shiftImmType      :: W.W 1
                         }
  deriving (Eq, Ord, Show)

instance PP.Pretty ShiftImm where
  pPrint m =
      -- This ShiftImm has only 1 bit of shift type, which is used as
      -- the upper bit of the normal 2-bit shift value.
      let ty = W.w (W.unW (shiftImmType m) * 2)
          amt = shiftImmImmediate m
      in PP.pPrint $ ShiftImmSpec ty amt

shiftImmImmField :: Field 5
shiftImmImmField = field 0

shiftImmTypeField :: Field 1
shiftImmTypeField = field 5

mkShiftImm :: Word32 -> ShiftImm
mkShiftImm w = ShiftImm imm ty
  where
    ty = extract shiftImmTypeField w
    imm = extract shiftImmImmField w

shiftImmToBits :: ShiftImm -> Word32
shiftImmToBits (ShiftImm imm ty) =
    insert shiftImmTypeField ty $
    insert shiftImmImmField imm 0

-- | Only used with MRS/MSR and indicative of CPSR/APSR.
data BankedReg = BankedReg
               deriving (Eq, Ord, Show)

instance PP.Pretty BankedReg where
    pPrint BankedReg = PP.text "cpsr"

mkBankedReg :: Word32 -> BankedReg
mkBankedReg = const BankedReg

bankedRegToBits :: BankedReg -> Word32
bankedRegToBits _ = 0

data SvcOperand = SvcOperand Word32
                deriving (Eq, Ord, Show)

instance PP.Pretty SvcOperand where
    pPrint (SvcOperand v) =
        let s = showHex v ""
            padding = replicate (8 - length s) '0'
        in PP.text $ if v < 0 then "-" else "" <> "0x" <> padding <> s

mkSvcOperand :: Word32 -> SvcOperand
mkSvcOperand = SvcOperand . fromIntegral

svcOperandToBits :: SvcOperand -> Word32
svcOperandToBits (SvcOperand v) = v

data RegWithAdd = RegWithAdd { regWithAddReg :: GPR
                             , regWithAddAdd :: W.W 1
                             }
  deriving (Eq, Ord, Show)

instance PP.Pretty RegWithAdd where
  pPrint m =
      let opStr = if regWithAddAdd m == 1 then mempty else PP.char '-'
      in (opStr <> PP.pPrint (regWithAddReg m))

regWithAddRegField :: Field 4
regWithAddRegField = field 0

regWithAddAddField :: Field 1
regWithAddAddField = field 4

mkRegWithAdd :: Word32 -> RegWithAdd
mkRegWithAdd w = RegWithAdd (GPR reg) add
  where
    add = extract regWithAddAddField w
    reg = extract regWithAddRegField w

regWithAddToBits :: RegWithAdd -> Word32
regWithAddToBits (RegWithAdd (GPR reg) add) =
    insert regWithAddAddField add $
    insert regWithAddRegField reg 0

-- | An Am2offset_imm memory reference
data Am2OffsetImm = Am2OffsetImm { am2OffsetImmAdd :: W.W 1
                                 , am2OffsetImmImmediate :: W.W 12
                                 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Am2OffsetImm where
  pPrint m =
      let s = PP.text $ if am2OffsetImmAdd m == 0 then "-" else ""
      in s <> (PP.pPrint (am2OffsetImmImmediate m))

am2OffsetImmAddField :: Field 1
am2OffsetImmAddField = field 12

am2OffsetImmImmField :: Field 12
am2OffsetImmImmField = field 0

mkAm2OffsetImm :: Word32 -> Am2OffsetImm
mkAm2OffsetImm w = Am2OffsetImm add imm
  where
    add = extract am2OffsetImmAddField w
    imm = extract am2OffsetImmImmField w

am2OffsetImmToBits :: Am2OffsetImm -> Word32
am2OffsetImmToBits (Am2OffsetImm a imm) =
    insert am2OffsetImmAddField a $
    insert am2OffsetImmImmField imm 0

-- | An am2offset_reg operand with a register, immediate, and shift type
-- (l/r). See also LDRBT in the ARM ARM and tgen.
data Am2OffsetReg = Am2OffsetReg { am2OffsetRegImmediate :: W.W 5
                                 , am2OffsetRegType      :: W.W 2
                                 , am2OffsetRegReg       :: GPR
                                 , am2OffsetRegAdd       :: W.W 1
                                 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Am2OffsetReg where
  pPrint m =
      let opStr = if am2OffsetRegAdd m == 1 then mempty else PP.char '-'
          shft = ShiftImmSpec (am2OffsetRegType m) (am2OffsetRegImmediate m)
      in (opStr <> PP.pPrint (am2OffsetRegReg m) <> PP.pPrint shft)

am2OffsetRegAddField :: Field 1
am2OffsetRegAddField = field 12

am2OffsetRegImmField :: Field 5
am2OffsetRegImmField = field 7

am2OffsetRegTypeField :: Field 2
am2OffsetRegTypeField = field 5

am2OffsetRegRegField :: Field 4
am2OffsetRegRegField = field 0

mkAm2OffsetReg :: Word32 -> Am2OffsetReg
mkAm2OffsetReg w = Am2OffsetReg imm ty (GPR reg) add
  where
    add = extract am2OffsetRegAddField w
    imm = extract am2OffsetRegImmField w
    ty  = extract am2OffsetRegTypeField w
    reg = extract am2OffsetRegRegField w

am2OffsetRegToBits :: Am2OffsetReg -> Word32
am2OffsetRegToBits (Am2OffsetReg imm ty (GPR reg) add) =
    insert am2OffsetRegAddField add $
    insert am2OffsetRegImmField imm $
    insert am2OffsetRegTypeField ty $
    insert am2OffsetRegRegField reg 0

-- | An AddrMode5 memory reference
data AddrMode5 = AddrMode5 { addrMode5Add       :: W.W 1
                           , addrMode5Immediate :: W.W 8
                           , addrMode5Reg       :: GPR
                           }
  deriving (Eq, Ord, Show)

instance PP.Pretty AddrMode5 where
  pPrint m = PP.brackets $
      let s = PP.text $ if addrMode5Add m == 1 then "" else "-"
          imm :: Word32
          imm = fromIntegral $ W.unW $ addrMode5Immediate m
      in PP.pPrint (addrMode5Reg m) <>
            (PP.char ',' PP.<+> (PP.char '#' <> s <>
             PP.pPrint (imm `shiftL` 2)))

addrMode5AddField :: Field 1
addrMode5AddField = field 8

addrMode5RegField :: Field 4
addrMode5RegField = field 9

addrMode5ImmField :: Field 8
addrMode5ImmField = field 0

mkAddrMode5 :: Word32 -> AddrMode5
mkAddrMode5 w = AddrMode5 add imm (GPR reg)
  where
    add = extract addrMode5AddField w
    imm = extract addrMode5ImmField w
    reg = extract addrMode5RegField w

addrMode5ToBits :: AddrMode5 -> Word32
addrMode5ToBits (AddrMode5 a imm (GPR reg)) =
    insert addrMode5AddField a $
    insert addrMode5ImmField imm $
    insert addrMode5RegField reg 0

-- | An load/store memory reference for a preload (e.g. PLDW)
-- instruction
data LdstSoReg = LdstSoReg { ldstSoRegBaseRegister   :: GPR
                           , ldstSoRegOffsetRegister :: GPR
                           , ldstSoRegAdd            :: W.W 1
                           , ldstSoRegImmediate      :: W.W 5
                           , ldstSoRegShiftType      :: W.W 2
                           }
  deriving (Eq, Ord, Show)

instance PP.Pretty LdstSoReg where
  pPrint m =
      let s = PP.text $ if ldstSoRegAdd m == 1 then "" else "-"
          shft = ShiftImmSpec (ldstSoRegShiftType m) (ldstSoRegImmediate m)
      in (PP.pPrint (ldstSoRegBaseRegister m) <> PP.char ',') PP.<+>
         (PP.pPrint (ldstSoRegOffsetRegister m) <> PP.pPrint shft)

ldstSoRegBaseRegField :: Field 4
ldstSoRegBaseRegField = field 13

ldstSoRegOffsetRegField :: Field 4
ldstSoRegOffsetRegField = field 0

ldstSoRegAddField :: Field 1
ldstSoRegAddField = field 12

ldstSoRegTypeField :: Field 2
ldstSoRegTypeField = field 5

ldstSoRegImmField :: Field 5
ldstSoRegImmField = field 7

mkLdstSoSreg :: Word32 -> LdstSoReg
mkLdstSoSreg w = LdstSoReg (GPR baseReg) (GPR offsetReg) add imm ty
  where
    baseReg   = extract ldstSoRegBaseRegField w
    offsetReg = extract ldstSoRegOffsetRegField w
    add       = extract ldstSoRegAddField w
    imm       = extract ldstSoRegImmField w
    ty        = extract ldstSoRegTypeField w

ldstSoRegToBits :: LdstSoReg -> Word32
ldstSoRegToBits (LdstSoReg (GPR baseR) (GPR offsetR) a imm ty) =
    insert ldstSoRegBaseRegField baseR $
    insert ldstSoRegOffsetRegField offsetR $
    insert ldstSoRegAddField a $
    insert ldstSoRegImmField imm $
    insert ldstSoRegTypeField ty 0

-- | A twelve-bit immediate
data ModImm = ModImm { modImmOrigImmediate  :: W.W 8
                     , modImmOrigRotate     :: W.W 4
                     }
  deriving (Eq, Ord, Show)

instance PP.Pretty ModImm where
  pPrint (ModImm imm rot) =
      let val :: Int32
          val = fromIntegral $ imm32 `rotateR` ((fromIntegral $ W.unW rot) * 2)

      -- See objump, opcodes/arm-dis.c, print_insn_arm, case 'o'.
          imm32 :: Word32
          imm32 = fromIntegral $ W.unW imm

          rotate = 2 * (fromIntegral $ W.unW rot)

          a = (((imm32 `shiftL` (32 - rotate))
              .|. (imm32 `shiftR` rotate)) .&. 0xffffffff)

          -- If there is another encoding with a smaller rotate, the
          -- rotate should be specified directly.
          smallerRotates = filter isSmallerRotate [0,2..30]

          isSmallerRotate i =
                ((a `shiftL` i) .|. (a `shiftR` (32 - i))) <= 0xff

          smallestRotate = case smallerRotates of
              (e:_) -> e
              _     -> rotate

      in if smallestRotate /= rotate
         then PP.text "#" <> (PP.pPrint imm) <> (PP.text "," PP.<+> PP.pPrint rotate)
         else PP.text "#" <> (PP.pPrint val)

modImmImmField :: Field 8
modImmImmField = field 0

modImmRotField :: Field 4
modImmRotField = field 8

-- See the ARM ARM, A5.2.4, Modified immediate constants in ARM
-- instructions
mkModImm :: Word32 -> ModImm
mkModImm w = ModImm imm rot
  where
    imm = extract modImmImmField w
    rot = extract modImmRotField w

modImmToBits :: ModImm -> Word32
modImmToBits (ModImm imm rot) =
    insert modImmImmField imm $
    insert modImmRotField rot 0

-- | A five-bit immediate
data Imm5 = Imm5 { unImm5 :: W.W 5
                 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Imm5 where
  pPrint v = PP.char '#' <> (PP.pPrint $ unImm5 v)

imm5Field :: Field 5
imm5Field = field 0

mkImm5 :: Word32 -> Imm5
mkImm5 w = Imm5 $ extract imm5Field w

imm5ToBits :: Imm5 -> Word32
imm5ToBits (Imm5 i) = insert imm5Field i 0

-- | A branch target
data BranchTarget = BranchTarget { unBranchTarget :: W.W 24
                                 }
  deriving (Eq, Ord, Show)

instance PP.Pretty BranchTarget where
  pPrint = PP.pPrint . unBranchTarget

branchTargetField :: Field 24
branchTargetField = field 0

mkBranchTarget :: Word32 -> BranchTarget
mkBranchTarget w = BranchTarget i
  where
    i = extract branchTargetField w

branchTargetToBits :: BranchTarget -> Word32
branchTargetToBits (BranchTarget i) =
    insert branchTargetField i 0

-- | A branch-and-execute target
data BranchExecuteTarget = BranchExecuteTarget { unBranchExecuteTarget :: W.W 25
                                               }
  deriving (Eq, Ord, Show)

instance PP.Pretty BranchExecuteTarget where
    pPrint (BranchExecuteTarget t) = PP.pPrint $ t `shiftL` 1

branchExecuteTargetField :: Field 25
branchExecuteTargetField = field 0

mkBranchExecuteTarget :: Word32 -> BranchExecuteTarget
mkBranchExecuteTarget w = BranchExecuteTarget $ extract branchExecuteTargetField w

branchExecuteTargetToBits :: BranchExecuteTarget -> Word32
branchExecuteTargetToBits (BranchExecuteTarget i) =
    insert branchExecuteTargetField i 0

data MSRMask = MSRMask { msrMaskWriteBit :: W.W 1
                       , msrMaskImmediate :: W.W 4
                       }
  deriving (Eq, Ord, Show)

instance PP.Pretty MSRMask where
    pPrint (MSRMask _ _) = PP.text "<todo>"

msrMaskWriteField :: Field 1
msrMaskWriteField = field 4

msrMaskImmField :: Field 4
msrMaskImmField = field 0

mkMSRMask :: Word32 -> MSRMask
mkMSRMask w = MSRMask wr imm
    where
    wr = extract msrMaskWriteField w
    imm = extract msrMaskImmField w

msrMaskToBits :: MSRMask -> Word32
msrMaskToBits (MSRMask wr i) =
    insert msrMaskWriteField wr $
    insert msrMaskImmField i 0

-- | A 16-bit immediate split into 12- and 4-bit chunks
data Imm16 = Imm16 { unImm16 :: Integer
                   }
  deriving (Eq, Ord, Show)

instance PP.Pretty Imm16 where
  pPrint (Imm16 i) = PP.integer i

mkImm16 :: Word32 -> Imm16
mkImm16 = Imm16 . fromIntegral

imm16ToBits :: Imm16 -> Word32
imm16ToBits (Imm16 i) = fromIntegral i

-- | Four-bit condition flag sequence
data Pred = Pred { unPred :: W.W 4
                 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Pred where
    -- See the ARM ARM DDI 0406C.b, A8.3 Conditional Execution
    pPrint (Pred 0b0000) = PP.text "eq"
    pPrint (Pred 0b0001) = PP.text "ne"
    pPrint (Pred 0b0010) = PP.text "cs"
    pPrint (Pred 0b0011) = PP.text "cc"
    pPrint (Pred 0b0100) = PP.text "mi"
    pPrint (Pred 0b0101) = PP.text "pl"
    pPrint (Pred 0b0110) = PP.text "vs"
    pPrint (Pred 0b0111) = PP.text "vc"
    pPrint (Pred 0b1000) = PP.text "hi"
    pPrint (Pred 0b1001) = PP.text "ls"
    pPrint (Pred 0b1010) = PP.text "ge"
    pPrint (Pred 0b1011) = PP.text "lt"
    pPrint (Pred 0b1100) = PP.text "gt"
    pPrint (Pred 0b1101) = PP.text "le"
    pPrint (Pred 0b1110) = PP.text ""
    pPrint _             = PP.text "<UND>"

mkPred :: Word32 -> Pred
mkPred = Pred . W.w . fromIntegral

predToBits :: Pred -> Word32
predToBits (Pred p) = fromIntegral $ W.unW p

-- | An ADR immediate offset and addition bit
data AdrLabel = AdrLabel { adrLabelImm :: W.W 12
                         , adrLabelOther :: W.W 2
                         }
  deriving (Eq, Ord, Show)

instance PP.Pretty AdrLabel where
  pPrint (AdrLabel imm _) = PP.pPrint imm

otherBitsField :: Field 2
otherBitsField = field 12

adrLabelImmField :: Field 12
adrLabelImmField = field 0

mkAdrLabel :: Word32 -> AdrLabel
mkAdrLabel w = AdrLabel i otherBits
  where
    i = extract adrLabelImmField w
    otherBits = extract otherBitsField w

adrLabelToBits :: AdrLabel -> Word32
adrLabelToBits (AdrLabel imm other) =
    insert otherBitsField other $
    insert adrLabelImmField imm 0

data SoRegImm = SoRegImm { soRegImmImmediate :: W.W 5
                         , soRegImmReg       :: GPR
                         , soRegImmShiftType :: W.W 2
                         }
  deriving (Eq, Ord, Show)

instance PP.Pretty SoRegImm where
    pPrint (SoRegImm imm reg ty) =
          PP.pPrint reg <> PP.pPrint (ShiftImmSpec ty imm)

soRegImmImmField :: Field 5
soRegImmImmField = field 7

soRegImmRegField :: Field 4
soRegImmRegField = field 0

soRegImmShiftTypeField :: Field 2
soRegImmShiftTypeField = field 5

mkSoRegImm :: Word32 -> SoRegImm
mkSoRegImm w = SoRegImm imm (GPR reg) ty
  where
    imm = extract soRegImmImmField w
    ty  = extract soRegImmShiftTypeField w
    reg = extract soRegImmRegField w

soRegImmToBits :: SoRegImm -> Word32
soRegImmToBits (SoRegImm imm (GPR reg) ty) =
    insert soRegImmImmField imm $
    insert soRegImmShiftTypeField ty $
    insert soRegImmRegField reg 0

data SoRegReg = SoRegReg { soRegRegReg1      :: GPR
                         , soRegRegReg2      :: GPR
                         , soRegRegShiftType :: W.W 2
                         }
  deriving (Eq, Ord, Show)

instance PP.Pretty SoRegReg where
    pPrint (SoRegReg reg1 reg2 ty) =
        (PP.pPrint reg1 <> PP.text ",") PP.<+> PP.pPrint (ShiftRegSpec ty reg2)

soRegRegReg2Field :: Field 4
soRegRegReg2Field = field 8

soRegRegReg1Field :: Field 4
soRegRegReg1Field = field 0

soRegRegShiftTypeField :: Field 2
soRegRegShiftTypeField = field 5

mkSoRegReg :: Word32 -> SoRegReg
mkSoRegReg w = SoRegReg (GPR reg1) (GPR reg2) ty
  where
    ty   = extract soRegRegShiftTypeField w
    reg1 = extract soRegRegReg1Field w
    reg2 = extract soRegRegReg2Field w

soRegRegToBits :: SoRegReg -> Word32
soRegRegToBits (SoRegReg (GPR reg1) (GPR reg2) ty) =
    insert soRegRegShiftTypeField ty $
    insert soRegRegReg1Field reg1 $
    insert soRegRegReg2Field reg2 0

newtype MemBarrierOpt = MemBarrierOpt (W.W 4)
  deriving (Eq, Ord, Show)

pattern SY = MemBarrierOpt 0b1111
pattern ST = MemBarrierOpt 0b1110
pattern ISH = MemBarrierOpt 0b1011
pattern ISHST = MemBarrierOpt 0b1010
pattern NSH = MemBarrierOpt 0b0111
pattern NSHST = MemBarrierOpt 0b0110
pattern OSH = MemBarrierOpt 0b0011
pattern OSHST = MemBarrierOpt 0b0010

instance PP.Pretty MemBarrierOpt where
  pPrint mbo =
    case mbo of
      SY -> PP.text "sy"
      ST -> PP.text "st"
      ISH -> PP.text "ish"
      ISHST -> PP.text "ishst"
      NSH -> PP.text "nsh"
      NSHST -> PP.text "nshst"
      OSH -> PP.text "osh"
      OSHST -> PP.text "oshst"
      MemBarrierOpt w -> PP.text "[unknown memory barrier option:" PP.<+> PP.int (fromIntegral $ W.unW w)

memBarrierOptField :: Field 4
memBarrierOptField = field 0

mkMemBarrierOpt :: Word32 -> MemBarrierOpt
mkMemBarrierOpt w = MemBarrierOpt i
  where
    i =  extract memBarrierOptField w

memBarrierOptToBits :: MemBarrierOpt -> Word32
memBarrierOptToBits (MemBarrierOpt w) =
  insert memBarrierOptField w 0

instance A.Arbitrary MemBarrierOpt where
  arbitrary g = A.oneof [SY, ST, ISH, ISHST, NSH, NSHST, OSH, OSHST] g

instance A.Arbitrary GPR where
  arbitrary g = GPR <$> A.arbitrary g

instance A.Arbitrary DPR where
  arbitrary g = DPR <$> A.arbitrary g

instance A.Arbitrary QPR where
  arbitrary g = QPR <$> A.arbitrary g

instance A.Arbitrary QQPR where
  arbitrary g = (QQPR . (*2)) <$> A.arbitrary g

instance A.Arbitrary AddrModeImm12 where
  arbitrary g = AddrModeImm12 <$> A.arbitrary g
                              <*> A.arbitrary g
                              <*> A.arbitrary g

instance A.Arbitrary CoprocRegister where
  arbitrary g = CoprocRegister <$> A.arbitrary g

instance A.Arbitrary Opcode7 where
  arbitrary g = Opcode7 <$> A.arbitrary g

instance A.Arbitrary Opcode15 where
  arbitrary g = Opcode15 <$> A.arbitrary g

instance A.Arbitrary AddrMode3 where
  arbitrary g = AddrMode3 <$> A.arbitrary g
                          <*> A.arbitrary g
                          <*> A.arbitrary g
                          <*> A.arbitrary g

instance A.Arbitrary AM3Offset where
  arbitrary g = AM3Offset <$> A.arbitrary g
                          <*> A.arbitrary g
                          <*> A.arbitrary g

-- FIXME: This is probably not right.  Not sure what the real range of the
-- stored immediate is
instance A.Arbitrary Imm8S4 where
  arbitrary g = Imm8S4 <$> A.arbitrary g <*> A.arbitrary g

-- FIXME: Probably also not right
instance A.Arbitrary ShiftImm where
  arbitrary g = ShiftImm <$> A.arbitrary g <*> A.arbitrary g

instance A.Arbitrary SvcOperand where
  arbitrary g = SvcOperand <$> A.arbitrary g

instance A.Arbitrary RegWithAdd where
  arbitrary g = RegWithAdd <$> A.arbitrary g <*> A.arbitrary g

instance A.Arbitrary Am2OffsetImm where
  arbitrary g = Am2OffsetImm <$> A.arbitrary g
                             <*> A.arbitrary g

instance A.Arbitrary Am2OffsetReg where
  arbitrary g = Am2OffsetReg <$> A.arbitrary g
                             <*> A.arbitrary g
                             <*> A.arbitrary g
                             <*> A.arbitrary g

instance A.Arbitrary AddrMode5 where
  arbitrary g = AddrMode5 <$> A.arbitrary g
                          <*> A.arbitrary g
                          <*> A.arbitrary g

instance A.Arbitrary LdstSoReg where
  arbitrary g = LdstSoReg <$> A.arbitrary g
                          <*> A.arbitrary g
                          <*> A.arbitrary g
                          <*> A.arbitrary g
                          <*> A.arbitrary g

instance A.Arbitrary ModImm where
  arbitrary g = ModImm <$> A.arbitrary g
                       <*> A.arbitrary g

instance A.Arbitrary Imm5 where
  arbitrary g = Imm5 <$> A.arbitrary g

instance A.Arbitrary BranchTarget where
  arbitrary g = BranchTarget <$> A.arbitrary g

instance A.Arbitrary BranchExecuteTarget where
  arbitrary g = BranchExecuteTarget <$> A.arbitrary g

instance A.Arbitrary MSRMask where
  arbitrary g = MSRMask <$> A.arbitrary g <*> A.arbitrary g

instance A.Arbitrary Imm16 where
  arbitrary g = (Imm16 . fromIntegral) <$> A.uniformR (minBound :: Int16, maxBound :: Int16) g

instance A.Arbitrary Pred where
  arbitrary g = Pred <$> A.arbitrary g

instance A.Arbitrary AdrLabel where
  arbitrary g = AdrLabel <$> A.arbitrary g <*> A.arbitrary g

instance A.Arbitrary SoRegImm where
  arbitrary g = SoRegImm <$> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g

instance A.Arbitrary SoRegReg where
  arbitrary g = SoRegReg <$> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g

instance A.Arbitrary Bit where
  arbitrary g = Bit <$> A.arbitrary g

instance A.Arbitrary SBit where
  arbitrary g = SBit <$> A.arbitrary g

instance A.Arbitrary Reglist where
  arbitrary g = Reglist <$> A.arbitrary g

instance A.Arbitrary BankedReg where
  arbitrary _ = return BankedReg
