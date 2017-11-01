{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE BinaryLiterals #-}
module Dismantle.Thumb.Operands (
  GPR,
  gpr,
  unGPR,

  LowGPR,
  lowGpr,
  unLowGPR,

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

  Opcode,
  mkOpcode,
  opcodeToBits,

  ShiftImm,
  mkShiftImm,
  shiftImmToBits,

  Imm8S4,
  mkImm8s4,
  imm8s4ToBits,

  RegWithAdd,
  mkRegWithAdd,
  regWithAddToBits,

  SvcOperand,
  mkSvcOperand,
  svcOperandToBits,

  AddrMode3,
  mkAddrMode3,
  addrMode3ToBits,

  AddrModeIs1,
  mkAddrModeIs1,
  addrModeIs1ToBits,

  AddrModeIs2,
  mkAddrModeIs2,
  addrModeIs2ToBits,

  AddrModeIs4,
  mkAddrModeIs4,
  addrModeIs4ToBits,

  AddrModePc,
  mkAddrModePc,
  addrModePcToBits,

  AddrModeRr,
  mkAddrModeRr,
  addrModeRrToBits,

  AM3Offset,
  mkAM3Offset,
  am3OffsetToBits,

  Am2OffsetReg,
  mkAm2OffsetReg,
  am2OffsetRegToBits,

  Am2OffsetImm,
  mkAm2OffsetImm,
  am2OffsetImmToBits,

  AddrMode5,
  mkAddrMode5,
  addrMode5ToBits,

  LdstSoReg,
  mkLdstSoSreg,
  ldstSoRegToBits,

  AddrModeImm12,
  mkAddrModeImm12,
  addrModeImm12ToBits,

  BranchTarget,
  mkBranchTarget,
  branchTargetToBits,

  BranchExecuteTarget,
  mkBranchExecuteTarget,
  branchExecuteTargetToBits,

  ModImm,
  mkModImm,
  modImmToBits,

  Imm5,
  mkImm5,
  imm5ToBits,

  Imm16,
  mkImm16,
  imm16ToBits,

  AdrLabel,
  mkAdrLabel,
  adrLabelToBits,

  Pred,
  mkPred,
  predToBits,

  MSRMask,
  mkMSRMask,
  msrMaskToBits,

  SoRegImm,
  mkSoRegImm,
  soRegImmToBits,

  SoRegReg,
  mkSoRegReg,
  soRegRegToBits
  ) where

import Data.Bits
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Word ( Word8, Word16, Word32 )
import Data.Int ( Int16, Int32 )

import Numeric (showHex)

import qualified Text.PrettyPrint.HughesPJClass as PP

import qualified Dismantle.Arbitrary as A
import Dismantle.Tablegen.TH.Pretty

-- | A bit field description and functions for using it

data Field = Field { fieldBits :: Int
                   -- ^ The number of bits in the field
                   , fieldOffset :: Int
                   -- ^ The offset of the rightmost bit in the field,
                   -- starting from zero
                   }

mkMask :: Field -> Word32
mkMask (Field bits offset) = (2 ^ bits - 1) `shiftL` offset

insert :: (Integral a) => Field -> a -> Word32 -> Word32
insert (Field bits offset) src dest =
    dest .|. (((fromIntegral src) .&. (2 ^ bits - 1)) `shiftL` offset)

extract :: Field -> Word32 -> Word32
extract f val = (val .&. (mkMask f)) `shiftR` (fieldOffset f)

-- Operand data types

newtype Bit = Bit { unBit :: Word8 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Bit where
  pPrint (Bit v) = PP.int $ fromIntegral v

mkBit :: Word32 -> Bit
mkBit v = Bit $ fromIntegral v

bitToBits :: Bit -> Word32
bitToBits (Bit v) = fromIntegral v

newtype SBit = SBit { unSBit :: Word8 }
  deriving (Eq, Ord, Show)

instance PP.Pretty SBit where
  pPrint (SBit 1) = PP.text "s"
  pPrint (SBit 0) = PP.text ""
  pPrint (SBit v) = error $ "Invalid SBit value: " <> show v

mkSBit :: Word32 -> SBit
mkSBit v = SBit $ fromIntegral v

sBitToBits :: SBit -> Word32
sBitToBits (SBit v) = fromIntegral v

data Reglist = Reglist { unReglist :: Word16 }
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
mkRegList v = Reglist $ fromIntegral v

regListToBits :: Reglist -> Word32
regListToBits (Reglist v) = fromIntegral v

-- | General-purpose low register by number (0-7) for 16-bit thumb
-- instructions
newtype LowGPR = LowGPR { unLowGPR :: Word8 }
  deriving (Eq, Ord, Show)

instance PP.Pretty LowGPR where
  pPrint (LowGPR rno) = PP.char 'r' <> PP.int (fromIntegral rno)

lowGpr :: Word8 -> LowGPR
lowGpr = LowGPR

-- | General-purpose register by number
newtype GPR = GPR { unGPR :: Word8 }
  deriving (Eq, Ord, Show)

instance PP.Pretty GPR where
  pPrint (GPR 10) = PP.text "sl"
  pPrint (GPR 11) = PP.text "fp"
  pPrint (GPR 12) = PP.text "ip"
  pPrint (GPR 13) = PP.text "sp"
  pPrint (GPR 14) = PP.text "lr"
  pPrint (GPR 15) = PP.text "pc"
  pPrint (GPR rno) = PP.char 'r' <> PP.int (fromIntegral rno)

gpr :: Word8 -> GPR
gpr = GPR

-- | Coprocessor register by number
newtype CoprocRegister = CoprocRegister { unCoprocRegister :: Word8 }
  deriving (Eq, Ord, Show)

mkCoprocRegister :: Word32 -> CoprocRegister
mkCoprocRegister w = CoprocRegister $ fromIntegral w

coprocRegisterToBits :: CoprocRegister -> Word32
coprocRegisterToBits (CoprocRegister i) = fromIntegral i

instance PP.Pretty CoprocRegister where
  pPrint (CoprocRegister r) = PP.text "cr" <> PP.pPrint r

-- | Coprocessor operation opcode register by number
newtype Opcode = Opcode { unOpcode :: Word8 }
  deriving (Eq, Ord, Show)

mkOpcode :: Word32 -> Opcode
mkOpcode w = Opcode $ fromIntegral w

opcodeToBits :: Opcode -> Word32
opcodeToBits (Opcode i) = fromIntegral i

instance PP.Pretty Opcode where
  pPrint (Opcode o) = PP.pPrint o

-- | Double-precision register by number
newtype DPR = DPR { unDPR :: Word8 }
  deriving (Eq, Ord, Show)

dpr :: Word8 -> DPR
dpr = DPR

instance PP.Pretty DPR where
  pPrint (DPR rno) = PP.char 'd' <> PP.int (fromIntegral rno)

-- | 128-bit vector register by number
newtype QPR = QPR { unQPR :: Word8 }
  deriving (Eq, Ord, Show)

qpr :: Word8 -> QPR
qpr = QPR

instance PP.Pretty QPR where
  pPrint (QPR rno) = PP.char 'q' <> PP.int (fromIntegral rno)

-- | 256-bit vector register (128-bit register pair) by number (must be
-- even)
newtype QQPR = QQPR { unQQPR :: Word8 }
  deriving (Eq, Ord, Show)

qqpr :: Word8 -> QQPR
qqpr = QQPR

instance PP.Pretty QQPR where
  pPrint (QQPR rno) = PP.char 'q' <> PP.int (fromIntegral rno)

-- | An AddrMode_Imm12 memory reference for a load or store instruction
-- (with a 12-bit immediate)
data AddrModeImm12 = AddrModeImm12 { addrModeImm12Register  :: GPR
                                   , addrModeImm12Immediate :: Word16
                                   , addrModeImm12Add       :: Word8
                                   }
  deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeImm12 where
  pPrint m =
      let s = PP.text $ if addrModeImm12Add m == 0 then "-" else ""
          suf = if addrModeImm12Immediate m == 0
                then mempty
                else PP.char ',' PP.<+> ((PP.char '#') <> (s <> (PP.pPrint $ addrModeImm12Immediate m)))
      in PP.brackets $ PP.pPrint (addrModeImm12Register m) <> suf

addrModeImm12RegField :: Field
addrModeImm12RegField = Field 4 13

addrModeImm12AddField :: Field
addrModeImm12AddField = Field 1 12

addrModeImm12ImmField :: Field
addrModeImm12ImmField = Field 12 0

mkAddrModeImm12 :: Word32 -> AddrModeImm12
mkAddrModeImm12 w = AddrModeImm12 (GPR $ fromIntegral reg) (fromIntegral imm) (fromIntegral add)
  where
    reg = extract addrModeImm12RegField w
    add = extract addrModeImm12AddField w
    imm = extract addrModeImm12ImmField w

addrModeImm12ToBits :: AddrModeImm12 -> Word32
addrModeImm12ToBits (AddrModeImm12 (GPR r) imm a) =
    insert addrModeImm12RegField r $
    insert addrModeImm12AddField a $
    insert addrModeImm12ImmField imm 0

data AddrModeIs1 =
    AddrModeIs1 { addrModeIs1Reg :: LowGPR
                , addrModeIs1Imm :: Word8
                }
                deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeIs1 where
  pPrint m =
      let suf = if addrModeIs1Imm m == 0
                then mempty
                else PP.char ',' PP.<+> ((PP.char '#') <> (PP.pPrint $ addrModeIs1Imm m))
      in PP.brackets $ PP.pPrint (addrModeIs1Reg m) <> suf

addrModeIs1RegField :: Field
addrModeIs1RegField = Field 3 0

addrModeIs1ImmField :: Field
addrModeIs1ImmField = Field 5 3

mkAddrModeIs1 :: Word32 -> AddrModeIs1
mkAddrModeIs1 w = AddrModeIs1 (LowGPR $ fromIntegral reg) (fromIntegral imm)
  where
    reg = extract addrModeIs1RegField w
    imm = extract addrModeIs1ImmField w

addrModeIs1ToBits :: AddrModeIs1 -> Word32
addrModeIs1ToBits (AddrModeIs1 (LowGPR r) imm) =
    insert addrModeIs1RegField r $
    insert addrModeIs1ImmField imm 0

data AddrModeIs2 =
    AddrModeIs2 { addrModeIs2Reg :: LowGPR
                , addrModeIs2Imm :: Word8
                }
                deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeIs2 where
  pPrint m =
      let suf = if addrModeIs2Imm m == 0
                then mempty
                else PP.char ',' PP.<+> ((PP.char '#') <> (PP.pPrint $ addrModeIs2Imm m))
      in PP.brackets $ PP.pPrint (addrModeIs2Reg m) <> suf

addrModeIs2RegField :: Field
addrModeIs2RegField = Field 3 0

addrModeIs2ImmField :: Field
addrModeIs2ImmField = Field 5 3

mkAddrModeIs2 :: Word32 -> AddrModeIs2
mkAddrModeIs2 w = AddrModeIs2 (LowGPR $ fromIntegral reg) (fromIntegral imm)
  where
    reg = extract addrModeIs2RegField w
    imm = extract addrModeIs2ImmField w

addrModeIs2ToBits :: AddrModeIs2 -> Word32
addrModeIs2ToBits (AddrModeIs2 (LowGPR r) imm) =
    insert addrModeIs2RegField r $
    insert addrModeIs2ImmField imm 0

data AddrModeIs4 =
    AddrModeIs4 { addrModeIs4Reg :: LowGPR
                , addrModeIs4Imm :: Word8
                }
                deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeIs4 where
  pPrint m =
      let suf = if addrModeIs4Imm m == 0
                then mempty
                else PP.char ',' PP.<+> ((PP.char '#') <> (PP.pPrint $ addrModeIs4Imm m))
      in PP.brackets $ PP.pPrint (addrModeIs4Reg m) <> suf

addrModeIs4RegField :: Field
addrModeIs4RegField = Field 3 0

addrModeIs4ImmField :: Field
addrModeIs4ImmField = Field 5 3

mkAddrModeIs4 :: Word32 -> AddrModeIs4
mkAddrModeIs4 w = AddrModeIs4 (LowGPR $ fromIntegral reg) (fromIntegral imm)
  where
    reg = extract addrModeIs4RegField w
    imm = extract addrModeIs4ImmField w

addrModeIs4ToBits :: AddrModeIs4 -> Word32
addrModeIs4ToBits (AddrModeIs4 (LowGPR r) imm) =
    insert addrModeIs4RegField r $
    insert addrModeIs4ImmField imm 0

data AddrModePc =
    AddrModePc { addrModePcImm :: Word8
               }
               deriving (Eq, Ord, Show)

instance PP.Pretty AddrModePc where
  pPrint m = PP.pPrint (((fromIntegral $ addrModePcImm m) :: Word32) `shiftL` 2)

addrModePcImmField :: Field
addrModePcImmField = Field 8 0

mkAddrModePc :: Word32 -> AddrModePc
mkAddrModePc w = AddrModePc (fromIntegral imm)
  where
    imm = extract addrModePcImmField w

addrModePcToBits :: AddrModePc -> Word32
addrModePcToBits (AddrModePc imm) =
    insert addrModeIs4ImmField imm 0

data AddrModeRr =
    AddrModeRr { addrModeRmReg :: LowGPR
               , addrModeRnReg :: LowGPR
               }
               deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeRr where
  pPrint (AddrModeRr rm rn) =
      PP.pPrint rm <> (PP.text ", " PP.<+> PP.pPrint rn)

addrModeRrRmField :: Field
addrModeRrRmField = Field 3 3

addrModeRrRnField :: Field
addrModeRrRnField = Field 3 0

mkAddrModeRr :: Word32 -> AddrModeRr
mkAddrModeRr w = AddrModeRr (LowGPR $ fromIntegral rm) (LowGPR $ fromIntegral rn)
  where
    rm = extract addrModeRrRmField w
    rn = extract addrModeRrRnField w

addrModeRrToBits :: AddrModeRr -> Word32
addrModeRrToBits (AddrModeRr (LowGPR rm) (LowGPR rn)) =
    insert addrModeRrRmField rm $
    insert addrModeRrRnField rn 0

-- | An AddrMode3 memory reference for a load or store instruction
data AddrMode3 = AddrMode3 { addrMode3Register  :: GPR
                           , addrMode3Immediate :: Word8
                           , addrMode3Type      :: Word8
                           , addrMode3Add       :: Word8
                           }
  deriving (Eq, Ord, Show)

instance PP.Pretty AddrMode3 where
  pPrint m =
      case addrMode3Type m of
          0 ->
              -- Interpret the low four bits of the immediate field as a
              -- register number (see bit 21 of LDRH variants, or bit 13
              -- of this operand)
              let r2 = GPR $ 0xf .&. (fromIntegral $ addrMode3Immediate m)
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

addrMode3RegField :: Field
addrMode3RegField = Field 4 9

addrMode3AddField :: Field
addrMode3AddField = Field 1 8

addrMode3TypeField :: Field
addrMode3TypeField = Field 1 13

addrMode3ImmField :: Field
addrMode3ImmField = Field 8 0

mkAddrMode3 :: Word32 -> AddrMode3
mkAddrMode3 w = AddrMode3 (GPR $ fromIntegral reg) (fromIntegral imm) (fromIntegral ty) (fromIntegral add)
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
data AM3Offset = AM3Offset { am3OffsetImmediate :: Word8
                           , am3OffsetOther     :: Word8
                           , am3OffsetAdd       :: Word8
                           }
  deriving (Eq, Ord, Show)

instance PP.Pretty AM3Offset where
  pPrint m =
      let r = GPR $ 0xf .&. (fromIntegral i)
          i = am3OffsetImmediate m
      in (if am3OffsetAdd m == 0 then PP.char '-' else mempty) <> PP.pPrint r

am3OffsetAddField :: Field
am3OffsetAddField = Field 1 8

am3OffsetOtherField :: Field
am3OffsetOtherField = Field 1 9

am3OffsetImmField :: Field
am3OffsetImmField = Field 8 0

mkAM3Offset :: Word32 -> AM3Offset
mkAM3Offset w = AM3Offset (fromIntegral imm) (fromIntegral other) (fromIntegral add)
  where
    add = extract am3OffsetAddField w
    imm = extract am3OffsetImmField w
    other = extract am3OffsetOtherField w

am3OffsetToBits :: AM3Offset -> Word32
am3OffsetToBits (AM3Offset imm other add) =
    insert am3OffsetAddField add $
    insert am3OffsetOtherField other $
    insert am3OffsetImmField imm 0

data ShiftType = LSL | LSR | ASR | ROR | RRX
               deriving (Eq, Ord, Show)

instance PP.Pretty ShiftType where
    pPrint LSL = PP.text "lsl"
    pPrint LSR = PP.text "lsr"
    pPrint ASR = PP.text "asr"
    pPrint ROR = PP.text "ror"
    pPrint RRX = PP.text "rrx"

decodeShiftType :: (Show a, Num a, Eq a) => a -> ShiftType
decodeShiftType v =
    case v of
        0b00 -> LSL
        0b01 -> LSR
        0b10 -> ASR
        0b11 -> ROR
        _    -> error $ "Invalid shift type bits: " <> show v

data Imm8S4 = Imm8S4 { imm8s4Add :: Word8
                     , imm8s4Immediate :: Word8
                     }
    deriving (Eq, Ord, Show)

instance PP.Pretty Imm8S4 where
    pPrint (Imm8S4 a i) =
        let s = PP.text $ if a == 1 then "" else "-"
        in PP.char '#' <> s <> PP.pPrint (((fromIntegral i) :: Word32) `shiftL` 2)

mkImm8s4 :: Word32 -> Imm8S4
mkImm8s4 w = Imm8S4 (fromIntegral add) (fromIntegral imm8)
  where
    add = extract (Field 1 8) w
    imm8 = extract (Field 8 0) w

imm8s4ToBits :: Imm8S4 -> Word32
imm8s4ToBits (Imm8S4 add imm) =
    insert (Field 1 8) add $
    insert (Field 8 0) imm 0

-- | A shift_imm operand with a shift immediate and shift type (l/r).
-- See also USAT in the ARM ARM and tgen.
data ShiftImm = ShiftImm { shiftImmImmediate :: Word8
                         , shiftImmType      :: Word8
                         }
  deriving (Eq, Ord, Show)

-- See ARM ARM A8.4.3, "Pseudocode details of instruction-specified
-- shifts and rotates"
decodeImmShift :: (Show a, Num a, Eq a, Num b, Eq b) => a -> b -> (ShiftType, b)
decodeImmShift ty imm =
    case decodeShiftType ty of
        LSL -> (LSL, imm)
        LSR -> (LSR, if imm == 0 then 32 else imm)
        ASR -> (ASR, if imm == 0 then 32 else imm)
        ROR ->
            if imm == 0
            then (RRX, 1)
            else (ROR, imm)
        _   -> error $ "Invalid shift type bits: " <> show ty

instance PP.Pretty ShiftImm where
  pPrint m =
      let (ty, amt) = decodeImmShift (shiftImmType m) (shiftImmImmediate m)
          addAmt = if ty == RRX && amt == 1
                   then id
                   else (PP.<+> PP.text ("#" <> show amt))

      in (addAmt $ PP.pPrint ty)

shiftImmImmField :: Field
shiftImmImmField = Field 5 0

shiftImmTypeField :: Field
shiftImmTypeField = Field 1 5

mkShiftImm :: Word32 -> ShiftImm
mkShiftImm w = ShiftImm (fromIntegral imm) (fromIntegral ty)
  where
    ty = extract shiftImmTypeField w
    imm = extract shiftImmImmField w

shiftImmToBits :: ShiftImm -> Word32
shiftImmToBits (ShiftImm imm ty) =
    insert shiftImmTypeField ty $
    insert shiftImmImmField imm 0

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
                             , regWithAddAdd :: Word8
                             }
  deriving (Eq, Ord, Show)

instance PP.Pretty RegWithAdd where
  pPrint m =
      let opStr = if regWithAddAdd m == 1 then mempty else PP.char '-'
      in (opStr <> PP.pPrint (regWithAddReg m))

regWithAddRegField :: Field
regWithAddRegField = Field 4 0

regWithAddAddField :: Field
regWithAddAddField = Field 1 4

mkRegWithAdd :: Word32 -> RegWithAdd
mkRegWithAdd w = RegWithAdd (GPR $ fromIntegral reg) (fromIntegral add)
  where
    add = extract regWithAddAddField w
    reg = extract regWithAddRegField w

regWithAddToBits :: RegWithAdd -> Word32
regWithAddToBits (RegWithAdd (GPR reg) add) =
    insert regWithAddAddField add $
    insert regWithAddRegField reg 0

-- | An Am2offset_imm memory reference
data Am2OffsetImm = Am2OffsetImm { am2OffsetImmAdd :: Word8
                                 , am2OffsetImmImmediate :: Word16
                                 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Am2OffsetImm where
  pPrint m =
      let s = PP.text $ if am2OffsetImmAdd m == 0 then "-" else ""
      in s <> (PP.pPrint (am2OffsetImmImmediate m))

am2OffsetImmAddField :: Field
am2OffsetImmAddField = Field 1 12

am2OffsetImmImmField :: Field
am2OffsetImmImmField = Field 12 0

mkAm2OffsetImm :: Word32 -> Am2OffsetImm
mkAm2OffsetImm w = Am2OffsetImm (fromIntegral add) (fromIntegral imm)
  where
    add = extract am2OffsetImmAddField w
    imm = extract am2OffsetImmImmField w

am2OffsetImmToBits :: Am2OffsetImm -> Word32
am2OffsetImmToBits (Am2OffsetImm a imm) =
    insert am2OffsetImmAddField a $
    insert am2OffsetImmImmField imm 0

-- | An am2offset_reg operand with a register, immediate, and shift type
-- (l/r). See also LDRBT in the ARM ARM and tgen.
data Am2OffsetReg = Am2OffsetReg { am2OffsetRegImmediate :: Word8
                                 , am2OffsetRegType      :: Word8
                                 , am2OffsetRegReg       :: GPR
                                 , am2OffsetRegAdd       :: Word8
                                 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Am2OffsetReg where
  pPrint m =
      let (t, imm) = decodeImmShift (am2OffsetRegType m) (am2OffsetRegImmediate m)
          addAmt = if t == RRX && imm == 1
                   then id
                   else (PP.<+> PP.text ("#" <> show imm))
          maybePrint = if imm == 0
                       then const mempty
                       else id
          opStr = if am2OffsetRegAdd m == 1 then mempty else PP.char '-'
      in (opStr <> PP.pPrint (am2OffsetRegReg m) <>
         (maybePrint $ addAmt $ PP.char ',' PP.<+> PP.pPrint t))

am2OffsetRegAddField :: Field
am2OffsetRegAddField = Field 1 12

am2OffsetRegImmField :: Field
am2OffsetRegImmField = Field 5 7

am2OffsetRegTypeField :: Field
am2OffsetRegTypeField = Field 2 5

am2OffsetRegRegField :: Field
am2OffsetRegRegField = Field 4 0

mkAm2OffsetReg :: Word32 -> Am2OffsetReg
mkAm2OffsetReg w = Am2OffsetReg (fromIntegral imm) (fromIntegral ty)
                                (GPR $ fromIntegral reg) (fromIntegral add)
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
data AddrMode5 = AddrMode5 { addrMode5Add       :: Word8
                           , addrMode5Immediate :: Word8
                           , addrMode5Reg       :: GPR
                           }
  deriving (Eq, Ord, Show)

instance PP.Pretty AddrMode5 where
  pPrint m = PP.brackets $
      let s = PP.text $ if addrMode5Add m == 1 then "" else "-"
          imm :: Word32
          imm = fromIntegral $ addrMode5Immediate m
      in PP.pPrint (addrMode5Reg m) <>
            (PP.char ',' PP.<+> (PP.char '#' <> s <>
             PP.pPrint (imm `shiftL` 2)))

addrMode5AddField :: Field
addrMode5AddField = Field 1 8

addrMode5RegField :: Field
addrMode5RegField = Field 4 9

addrMode5ImmField :: Field
addrMode5ImmField = Field 8 0

mkAddrMode5 :: Word32 -> AddrMode5
mkAddrMode5 w = AddrMode5 (fromIntegral add) (fromIntegral imm) (GPR $ fromIntegral reg)
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
                           , ldstSoRegAdd            :: Word8
                           , ldstSoRegImmediate      :: Word16
                           , ldstSoRegShiftType      :: Word8
                           }
  deriving (Eq, Ord, Show)

instance PP.Pretty LdstSoReg where
  pPrint m =
      let (t, amt) = decodeImmShift (ldstSoRegShiftType m) (ldstSoRegImmediate m)
          s = PP.text $ if ldstSoRegAdd m == 1 then "" else "-"
          addAmt = if t == RRX && amt == 1
                   then id
                   else (PP.<+> PP.text ("#" <> show amt))
          maybePrint = if amt == 0
                       then const mempty
                       else id
      in (PP.pPrint (ldstSoRegBaseRegister m) <> PP.char ',') PP.<+>
         (PP.pPrint (ldstSoRegOffsetRegister m) <>
          (maybePrint $ addAmt $ PP.text "," PP.<+> (s <> PP.pPrint t)))

ldstSoRegBaseRegField :: Field
ldstSoRegBaseRegField = Field 4 13

ldstSoRegOffsetRegField :: Field
ldstSoRegOffsetRegField = Field 4 0

ldstSoRegAddField :: Field
ldstSoRegAddField = Field 1 12

ldstSoRegTypeField :: Field
ldstSoRegTypeField = Field 2 5

ldstSoRegImmField :: Field
ldstSoRegImmField = Field 5 7

mkLdstSoSreg :: Word32 -> LdstSoReg
mkLdstSoSreg w = LdstSoReg (GPR $ fromIntegral baseReg)
                           (GPR $ fromIntegral offsetReg)
                           (fromIntegral add)
                           (fromIntegral imm)
                           (fromIntegral ty)
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
data ModImm = ModImm { modImmOrigImmediate  :: Word8
                     , modImmOrigRotate     :: Word8
                     }
  deriving (Eq, Ord, Show)

instance PP.Pretty ModImm where
  pPrint (ModImm imm rot) =
      let val :: Int32
          val = fromIntegral $ (fromIntegral imm :: Word32) `rotateR` ((fromIntegral rot) * 2)

      -- See objump, opcodes/arm-dis.c, print_insn_arm, case 'o'.
          imm32 :: Word32
          imm32 = fromIntegral imm

          rotate = 2 * fromIntegral rot

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

modImmImmField :: Field
modImmImmField = Field 8 0

modImmRotField :: Field
modImmRotField = Field 4 8

-- See the ARM ARM, A5.2.4, Modified immediate constants in ARM
-- instructions
mkModImm :: Word32 -> ModImm
mkModImm w = ModImm (fromIntegral imm) (fromIntegral rot)
  where
    imm = extract modImmImmField w
    rot = extract modImmRotField w

modImmToBits :: ModImm -> Word32
modImmToBits (ModImm imm rot) =
    insert modImmImmField imm $
    insert modImmRotField rot 0

-- | A five-bit immediate
data Imm5 = Imm5 { unImm5 :: Word8
                 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Imm5 where
  pPrint = PP.pPrint . unImm5

imm5Field :: Field
imm5Field = Field 5 0

mkImm5 :: Word32 -> Imm5
mkImm5 w = Imm5 $ fromIntegral i
  where
    i = extract imm5Field w

imm5ToBits :: Imm5 -> Word32
imm5ToBits (Imm5 i) =
    insert imm5Field (fromIntegral i) 0

-- | A branch target
data BranchTarget = BranchTarget { unBranchTarget :: Integer
                                 }
  deriving (Eq, Ord, Show)

instance PP.Pretty BranchTarget where
  pPrint = PP.pPrint . unBranchTarget

branchTargetField :: Field
branchTargetField = Field 24 0

mkBranchTarget :: Word32 -> BranchTarget
mkBranchTarget w = BranchTarget $ fromIntegral i
  where
    i = extract branchTargetField w

branchTargetToBits :: BranchTarget -> Word32
branchTargetToBits (BranchTarget i) =
    insert branchTargetField (fromIntegral i) 0

-- | A branch-and-execute target
data BranchExecuteTarget = BranchExecuteTarget { unBranchExecuteTarget :: Word32
                                               }
  deriving (Eq, Ord, Show)

instance PP.Pretty BranchExecuteTarget where
    pPrint (BranchExecuteTarget t) = PP.pPrint $ t `shiftL` 1

branchExecuteTargetField :: Field
branchExecuteTargetField = Field 25 0

mkBranchExecuteTarget :: Word32 -> BranchExecuteTarget
mkBranchExecuteTarget w = BranchExecuteTarget w

branchExecuteTargetToBits :: BranchExecuteTarget -> Word32
branchExecuteTargetToBits (BranchExecuteTarget i) =
    insert branchExecuteTargetField ((fromIntegral i)::Word32) 0

data MSRMask = MSRMask { msrMaskWriteBit :: Word8
                       , msrMaskImmediate :: Word8
                       }
  deriving (Eq, Ord, Show)

instance PP.Pretty MSRMask where
    pPrint (MSRMask _ _) = PP.text "<todo>"

msrMaskWriteField :: Field
msrMaskWriteField = Field 1 4

msrMaskImmField :: Field
msrMaskImmField = Field 4 0

mkMSRMask :: Word32 -> MSRMask
mkMSRMask w = MSRMask (fromIntegral wr) (fromIntegral imm)
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
data Pred = Pred { unPred :: Word8
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
mkPred = Pred . fromIntegral

predToBits :: Pred -> Word32
predToBits (Pred p) = fromIntegral p

-- | An ADR immediate offset and addition bit
data AdrLabel = AdrLabel { adrLabelImm :: Word16
                         , adrLabelOther :: Word8
                         }
  deriving (Eq, Ord, Show)

instance PP.Pretty AdrLabel where
  pPrint (AdrLabel imm _) = PP.pPrint imm

otherBitsField :: Field
otherBitsField = Field 2 12

adrLabelImmField :: Field
adrLabelImmField = Field 12 0

mkAdrLabel :: Word32 -> AdrLabel
mkAdrLabel w = AdrLabel (fromIntegral i) (fromIntegral otherBits)
  where
    i = extract adrLabelImmField w
    otherBits = extract otherBitsField w

adrLabelToBits :: AdrLabel -> Word32
adrLabelToBits (AdrLabel imm other) =
    insert otherBitsField other $
    insert adrLabelImmField imm 0

data SoRegImm = SoRegImm { soRegImmImmediate :: Word8
                         , soRegImmReg       :: GPR
                         , soRegImmShiftType :: Word8
                         }
  deriving (Eq, Ord, Show)

instance PP.Pretty SoRegImm where
    pPrint (SoRegImm imm reg ty) =
        let (t, amt) = decodeImmShift ty imm
            addAmt = if t == RRX && amt == 1
                     then id
                     else (PP.<+> PP.text ("#" <> show amt))
            maybePrint = if amt == 0
                         then const mempty
                         else id
        in PP.pPrint reg <>
           (maybePrint $ addAmt $ PP.text "," PP.<+> PP.pPrint t)

soRegImmImmField :: Field
soRegImmImmField = Field 5 7

soRegImmRegField :: Field
soRegImmRegField = Field 4 0

soRegImmShiftTypeField :: Field
soRegImmShiftTypeField = Field 2 5

mkSoRegImm :: Word32 -> SoRegImm
mkSoRegImm w = SoRegImm (fromIntegral imm) (GPR $ fromIntegral reg) (fromIntegral ty)
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
                         , soRegRegShiftType :: Word8
                         }
  deriving (Eq, Ord, Show)

instance PP.Pretty SoRegReg where
    pPrint (SoRegReg reg1 reg2 ty) =
        let t = decodeShiftType ty
        in PP.pPrint reg1 <> (PP.text "," PP.<+> PP.pPrint t PP.<+> (PP.pPrint reg2))

soRegRegReg2Field :: Field
soRegRegReg2Field = Field 4 8

soRegRegReg1Field :: Field
soRegRegReg1Field = Field 4 0

soRegRegShiftTypeField :: Field
soRegRegShiftTypeField = Field 2 5

mkSoRegReg :: Word32 -> SoRegReg
mkSoRegReg w = SoRegReg (GPR $ fromIntegral reg1) (GPR $ fromIntegral reg2) (fromIntegral ty)
  where
    ty   = extract soRegRegShiftTypeField w
    reg1 = extract soRegRegReg1Field w
    reg2 = extract soRegRegReg2Field w

soRegRegToBits :: SoRegReg -> Word32
soRegRegToBits (SoRegReg (GPR reg1) (GPR reg2) ty) =
    insert soRegRegShiftTypeField ty $
    insert soRegRegReg1Field reg1 $
    insert soRegRegReg2Field reg2 0

instance A.Arbitrary GPR where
  arbitrary g = GPR <$> A.uniformR (0, 15) g

instance A.Arbitrary DPR where
  arbitrary g = DPR <$> A.uniformR (0, 31) g

instance A.Arbitrary QPR where
  arbitrary g = QPR <$> A.uniformR (0, 15) g

instance A.Arbitrary QQPR where
  arbitrary g = (QQPR . (*2)) <$> A.uniformR (0, 7) g

instance A.Arbitrary AddrModeImm12 where
  arbitrary g = AddrModeImm12 <$> A.arbitrary g
                              <*> A.arbitrary g
                              <*> A.arbitrary g

instance A.Arbitrary CoprocRegister where
  arbitrary g = CoprocRegister <$> A.uniformR (0, 15) g

instance A.Arbitrary Opcode where
  arbitrary g = Opcode <$> A.uniformR (0, 7) g

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
  arbitrary g = Imm5 <$> A.uniformR (0, 31) g

instance A.Arbitrary BranchTarget where
  arbitrary g = BranchTarget <$> A.arbitrary g

instance A.Arbitrary BranchExecuteTarget where
  arbitrary g = BranchExecuteTarget <$> A.arbitrary g

instance A.Arbitrary MSRMask where
  arbitrary g = MSRMask <$> A.arbitrary g <*> A.arbitrary g

instance A.Arbitrary Imm16 where
  arbitrary g = (Imm16 . fromIntegral) <$> A.uniformR (minBound :: Int16, maxBound :: Int16) g

instance A.Arbitrary Pred where
  arbitrary g = Pred <$> A.uniformR (0, 16) g

instance A.Arbitrary AdrLabel where
  arbitrary g = AdrLabel <$> A.arbitrary g <*> A.arbitrary g

instance A.Arbitrary SoRegImm where
  arbitrary g = SoRegImm <$> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g

instance A.Arbitrary SoRegReg where
  arbitrary g = SoRegReg <$> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g

instance A.Arbitrary Bit where
  arbitrary g = Bit <$> A.uniformR (0, 1) g

instance A.Arbitrary SBit where
  arbitrary g = SBit <$> A.uniformR (0, 1) g

instance A.Arbitrary Reglist where
  arbitrary g = Reglist <$> A.arbitrary g
