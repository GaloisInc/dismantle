{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE BinaryLiterals #-}
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

  Opcode,
  mkOpcode,
  opcodeToBits,

  ShiftImm,
  mkShiftImm,
  shiftImmToBits,

  RegWithAdd,
  mkRegWithAdd,
  regWithAddToBits,

  AddrMode3,
  mkAddrMode3,
  addrMode3ToBits,

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
import Data.Int (Int32)

import qualified Text.PrettyPrint.HughesPJClass as PP

import Dismantle.Tablegen.TH.Pretty

-- | A bit field description and functions for using it

data Field = Field { fieldBits :: Int
                   -- ^ The number of bits in the field
                   , fieldOffset :: Int
                   -- ^ The offset of the rightmost bit in the field,
                   -- starting from zero
                   }

addBitToSign :: (Num a) => Word32 -> a
addBitToSign 1 = fromIntegral 1
addBitToSign 0 = fromIntegral (-1)
addBitToSign v = error $ "Invalid add bit value (word): " <> show v

addBitFromNum :: (Ord a, Num a) => a -> Word32
addBitFromNum v = if v < 0 then 0 else 1

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
  pPrint (CoprocRegister r) = PP.char 'p' <> PP.pPrint r

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
                                   , addrModeImm12Immediate :: Integer
                                   }
  deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeImm12 where
  pPrint m =
      let suf = if addrModeImm12Immediate m == 0
                then mempty
                else PP.char ',' PP.<+> ((PP.char '#') <> (PP.pPrint $ addrModeImm12Immediate m))
      in PP.brackets $ PP.pPrint (addrModeImm12Register m) <> suf

addrModeImm12RegField :: Field
addrModeImm12RegField = Field 4 13

addrModeImm12AddField :: Field
addrModeImm12AddField = Field 1 12

addrModeImm12ImmField :: Field
addrModeImm12ImmField = Field 12 0

mkAddrModeImm12 :: Word32 -> AddrModeImm12
mkAddrModeImm12 w = AddrModeImm12 (GPR $ fromIntegral reg) (addBitToSign add * fromIntegral imm)
  where
    reg = extract addrModeImm12RegField w
    add = extract addrModeImm12AddField w
    imm = extract addrModeImm12ImmField w

addrModeImm12ToBits :: AddrModeImm12 -> Word32
addrModeImm12ToBits (AddrModeImm12 (GPR r) imm) =
    insert addrModeImm12RegField r $
    insert addrModeImm12AddField (addBitFromNum imm) $
    insert addrModeImm12ImmField (abs imm) 0

-- | An AddrMode3 memory reference for a load or store instruction
data AddrMode3 = AddrMode3 { addrMode3Register  :: GPR
                           , addrMode3Immediate :: Integer
                           , addrMode3Type      :: Word8
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
              in PP.brackets $
                 PP.pPrint (addrMode3Register m) <> (PP.char ',' PP.<+> PP.pPrint r2)
          1 ->
              -- Interpet all bits of the immediate as an immediate
              let addImm = if addrMode3Immediate m == 0
                           then id
                           else (<> (PP.char ',' PP.<+> (PP.char '#' <> (PP.pPrint (addrMode3Immediate m)))))
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
mkAddrMode3 w = AddrMode3 (GPR $ fromIntegral reg) (addBitToSign add * fromIntegral imm) (fromIntegral ty)
  where
    reg = extract addrMode3RegField w
    add = extract addrMode3AddField w
    imm = extract addrMode3ImmField w
    ty  = extract addrMode3TypeField w

addrMode3ToBits :: AddrMode3 -> Word32
addrMode3ToBits (AddrMode3 (GPR r) imm other) =
    insert addrMode3RegField r $
    insert addrMode3AddField (addBitFromNum imm) $
    insert addrMode3ImmField (abs imm) $
    insert addrMode3TypeField other 0

-- | An am3Offset memory reference for a load or store instruction
data AM3Offset = AM3Offset { am3OffsetImmediate :: Integer
                           , am3OffsetOther     :: Word8
                           }
  deriving (Eq, Ord, Show)

instance PP.Pretty AM3Offset where
  pPrint m = PP.pPrint (am3OffsetImmediate m)

am3OffsetAddField :: Field
am3OffsetAddField = Field 1 8

am3OffsetOtherField :: Field
am3OffsetOtherField = Field 1 9

am3OffsetImmField :: Field
am3OffsetImmField = Field 8 0

mkAM3Offset :: Word32 -> AM3Offset
mkAM3Offset w = AM3Offset (addBitToSign add * fromIntegral imm) (fromIntegral other)
  where
    add = extract am3OffsetAddField w
    imm = extract am3OffsetImmField w
    other = extract am3OffsetOtherField w

am3OffsetToBits :: AM3Offset -> Word32
am3OffsetToBits (AM3Offset imm other) =
    insert am3OffsetAddField (addBitFromNum imm) $
    insert am3OffsetOtherField other $
    insert am3OffsetImmField (abs imm) 0

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
data Am2OffsetImm = Am2OffsetImm { am2OffsetImmImmediate :: Integer
                                 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Am2OffsetImm where
  pPrint m = PP.pPrint (am2OffsetImmImmediate m)

am2OffsetImmAddField :: Field
am2OffsetImmAddField = Field 1 12

am2OffsetImmImmField :: Field
am2OffsetImmImmField = Field 12 0

mkAm2OffsetImm :: Word32 -> Am2OffsetImm
mkAm2OffsetImm w = Am2OffsetImm (addBitToSign add * fromIntegral imm)
  where
    add = extract am2OffsetImmAddField w
    imm = extract am2OffsetImmImmField w

am2OffsetImmToBits :: Am2OffsetImm -> Word32
am2OffsetImmToBits (Am2OffsetImm imm) =
    insert am2OffsetImmAddField (addBitFromNum imm) $
    insert am2OffsetImmImmField (abs imm) 0

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
data AddrMode5 = AddrMode5 { addrMode5Immediate :: Integer
                           , addrMode5Reg       :: Word8
                           }
  deriving (Eq, Ord, Show)

instance PP.Pretty AddrMode5 where
  pPrint m = PP.pPrint (addrMode5Immediate m)

addrMode5AddField :: Field
addrMode5AddField = Field 1 8

addrMode5RegField :: Field
addrMode5RegField = Field 4 9

addrMode5ImmField :: Field
addrMode5ImmField = Field 8 0

mkAddrMode5 :: Word32 -> AddrMode5
mkAddrMode5 w = AddrMode5 (addBitToSign add * fromIntegral imm) (fromIntegral reg)
  where
    add = extract addrMode5AddField w
    imm = extract addrMode5ImmField w
    reg = extract addrMode5RegField w

addrMode5ToBits :: AddrMode5 -> Word32
addrMode5ToBits (AddrMode5 imm reg) =
    insert addrMode5AddField (addBitFromNum imm) $
    insert addrMode5ImmField (abs imm) $
    insert addrMode5RegField reg 0

-- | An load/store memory reference for a preload (e.g. PLDW)
-- instruction
data LdstSoReg = LdstSoReg { ldstSoRegBaseRegister   :: GPR
                           , ldstSoRegOffsetRegister :: GPR
                           , ldstSoRegImmediate      :: Integer
                           }
  deriving (Eq, Ord, Show)

instance PP.Pretty LdstSoReg where
  pPrint m =
      let addImm = if ldstSoRegImmediate m == 0
                   then id
                   else (<> (PP.char ',' PP.<+> (PP.pPrint (ldstSoRegImmediate m))))
      in (PP.pPrint (ldstSoRegBaseRegister m) <> PP.char ',') PP.<+>
         (addImm (PP.pPrint (ldstSoRegOffsetRegister m)))

ldstSoRegBaseRegField :: Field
ldstSoRegBaseRegField = Field 4 13

ldstSoRegOffsetRegField :: Field
ldstSoRegOffsetRegField = Field 4 0

ldstSoRegAddField :: Field
ldstSoRegAddField = Field 1 12

ldstSoRegImmField :: Field
ldstSoRegImmField = Field 5 7

mkLdstSoSreg :: Word32 -> LdstSoReg
mkLdstSoSreg w = LdstSoReg (GPR $ fromIntegral baseReg)
                           (GPR $ fromIntegral offsetReg)
                           (addBitToSign add * fromIntegral imm)
  where
    baseReg   = extract ldstSoRegBaseRegField w
    offsetReg = extract ldstSoRegOffsetRegField w
    add       = extract ldstSoRegAddField w
    imm       = extract ldstSoRegImmField w

ldstSoRegToBits :: LdstSoReg -> Word32
ldstSoRegToBits (LdstSoReg (GPR baseR) (GPR offsetR) imm) =
    insert ldstSoRegBaseRegField baseR $
    insert ldstSoRegOffsetRegField offsetR $
    insert ldstSoRegAddField (addBitFromNum imm) $
    insert ldstSoRegImmField (abs imm) 0

-- | A twelve-bit immediate
data ModImm = ModImm { modImmRecoveredValue :: Int32
                     , modImmOrigImmediate  :: Word8
                     , modImmOrigRotate     :: Word8
                     }
  deriving (Eq, Ord, Show)

instance PP.Pretty ModImm where
  pPrint (ModImm val _ _) = PP.text "#" <> (PP.pPrint val)

modImmImmField :: Field
modImmImmField = Field 8 0

modImmRotField :: Field
modImmRotField = Field 4 8

-- See the ARM ARM, A5.2.4, Modified immediate constants in ARM
-- instructions
mkModImm :: Word32 -> ModImm
mkModImm w = ModImm (fromIntegral val) (fromIntegral imm) (fromIntegral rot)
  where
    val = fromIntegral (imm `rotateR` ((fromIntegral rot) * 2))
    imm = extract modImmImmField w
    rot = extract modImmRotField w

modImmToBits :: ModImm -> Word32
modImmToBits (ModImm _ imm rot) =
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
data BranchExecuteTarget = BranchExecuteTarget { unBranchExecuteTarget :: Integer
                                               }
  deriving (Eq, Ord, Show)

instance PP.Pretty BranchExecuteTarget where
    pPrint (BranchExecuteTarget t) = PP.pPrint t

branchExecuteTargetField1 :: Field
branchExecuteTargetField1 = Field 24 0

branchExecuteTargetField2 :: Field
branchExecuteTargetField2 = Field 1 24

mkBranchExecuteTarget :: Word32 -> BranchExecuteTarget
mkBranchExecuteTarget w = BranchExecuteTarget $ fromIntegral $ w `shiftL` 1

branchExecuteTargetToBits :: BranchExecuteTarget -> Word32
branchExecuteTargetToBits (BranchExecuteTarget i) =
    insert branchExecuteTargetField1 (i `shiftR` 1) 0

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
data AdrLabel = AdrLabel { adrLabelImm :: Integer
                         }
  deriving (Eq, Ord, Show)

instance PP.Pretty AdrLabel where
  pPrint (AdrLabel imm) = PP.pPrint imm

addBitsField :: Field
addBitsField = Field 2 12

adrLabelImmField :: Field
adrLabelImmField = Field 12 0

mkAdrLabel :: Word32 -> AdrLabel
mkAdrLabel w = AdrLabel ((if addBits == 0b10 then 1 else (-1)) * fromIntegral i)
  where
    i = extract adrLabelImmField w
    addBits = extract addBitsField w

adrLabelToBits :: AdrLabel -> Word32
adrLabelToBits (AdrLabel imm) =
    insert addBitsField (if imm < 0 then 0b1 else 0b10) $
    insert adrLabelImmField (abs imm) 0

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
