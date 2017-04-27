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

  Bit,
  mkBit,
  bitToBits,

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

  Imm12,
  mkImm12,
  imm12ToBits,

  Imm5,
  mkImm5,
  imm5ToBits,

  Imm16,
  mkImm16,
  imm16ToBits,

  SBit,
  mkSBit,
  sBitToBits,

  AdrLabel,
  mkAdrLabel,
  adrLabelToBits,

  Pred,
  mkPred,
  predToBits
  ) where

import Data.Bits
import Data.Monoid
import Data.Word ( Word8, Word16, Word32 )

import qualified Text.PrettyPrint.HughesPJClass as PP

import Dismantle.Tablegen.TH.Pretty

newtype Bit = Bit { unBit :: Bool }
  deriving (Eq, Ord, Show)

mkBit :: Word32 -> Bit
mkBit 0 = Bit False
mkBit 1 = Bit True
mkBit v = error $ "Unexpected Bit value: " <> show v

bitToBits :: Bit -> Word32
bitToBits (Bit True ) = 1
bitToBits (Bit False) = 0

-- | General-purpose register by number
newtype GPR = GPR { unGPR :: Word8 }
  deriving (Eq, Ord, Show)

gpr :: Word8 -> GPR
gpr = GPR

-- | Coprocessor register by number
newtype CoprocRegister = CoprocRegister { unCoprocRegister :: Word8 }
  deriving (Eq, Ord, Show)

-- | Coprocessor operation opcode register by number
newtype Opcode = Opcode { unOpcode :: Word8 }
  deriving (Eq, Ord, Show)

-- | Double-precision register by number
newtype DPR = DPR { unDPR :: Word8 }
  deriving (Eq, Ord, Show)

dpr :: Word8 -> DPR
dpr = DPR

-- | 128-bit vector register by number
newtype QPR = QPR { unQPR :: Word8 }
  deriving (Eq, Ord, Show)

qpr :: Word8 -> QPR
qpr = QPR

-- | 256-bit vector register (128-bit register pair) by number (must be
-- even)
newtype QQPR = QQPR { unQQPR :: Word8 }
  deriving (Eq, Ord, Show)

qqpr :: Word8 -> QQPR
qqpr = QQPR

data Field = Field { fieldBits :: Int
                   , fieldOffset :: Int
                   }

addrModeImm12RegField :: Field
addrModeImm12RegField = Field 4 13

addrModeImm12AddField :: Field
addrModeImm12AddField = Field 1 12

addrModeImm12ImmField :: Field
addrModeImm12ImmField = Field 12 0

mkAddrModeImm12 :: Word32 -> AddrModeImm12
mkAddrModeImm12 w = AddrModeImm12 (GPR $ fromIntegral reg) (fromIntegral imm) (add == 1)
  where
    reg = extract addrModeImm12RegField w
    add = extract addrModeImm12AddField w
    imm = extract addrModeImm12ImmField w

addrModeImm12ToBits :: AddrModeImm12 -> Word32
addrModeImm12ToBits (AddrModeImm12 (GPR r) imm add) =
    insert addrModeImm12RegField r $
    insert addrModeImm12AddField (if add then 1 else 0) $
    insert addrModeImm12ImmField imm 0

addrMode3RegField :: Field
addrMode3RegField = Field 4 9

addrMode3AddField :: Field
addrMode3AddField = Field 1 8

addrMode3ImmField :: Field
addrMode3ImmField = Field 8 0

mkAddrMode3 :: Word32 -> AddrMode3
mkAddrMode3 w = AddrMode3 (GPR $ fromIntegral reg) (fromIntegral imm) (add == 1)
  where
    reg = extract addrMode3RegField w
    add = extract addrMode3AddField w
    imm = extract addrMode3ImmField w

addrMode3ToBits :: AddrMode3 -> Word32
addrMode3ToBits (AddrMode3 (GPR r) imm add) =
    insert addrMode3RegField r $
    insert addrMode3AddField (if add then 1 else 0) $
    insert addrMode3ImmField imm $
    -- Always set bit position 13 (see the tgen data and ARM ARM for
    -- LDRD etc. that use this operand type).
    insert (Field 1 13) 1 0

am3OffsetAddField :: Field
am3OffsetAddField = Field 1 8

am3OffsetImmField :: Field
am3OffsetImmField = Field 8 0

mkAM3Offset :: Word32 -> AM3Offset
mkAM3Offset w = AM3Offset (fromIntegral imm) (add == 1)
  where
    add = extract am3OffsetAddField w
    imm = extract am3OffsetImmField w

am3OffsetToBits :: AM3Offset -> Word32
am3OffsetToBits (AM3Offset imm add) =
    insert am3OffsetAddField (if add then 1 else 0) $
    insert am3OffsetImmField imm $
    -- Always set bit position 9 (see the tgen data and ARM ARM for STRD
    -- etc. that use this operand type).
    insert (Field 1 9) 1 0

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

regWithAddRegField :: Field
regWithAddRegField = Field 4 0

regWithAddAddField :: Field
regWithAddAddField = Field 1 4

mkRegWithAdd :: Word32 -> RegWithAdd
mkRegWithAdd w = RegWithAdd (GPR $ fromIntegral reg) (add == 1)
  where
    add = extract regWithAddAddField w
    reg = extract regWithAddRegField w

regWithAddToBits :: RegWithAdd -> Word32
regWithAddToBits (RegWithAdd (GPR reg) add) =
    insert regWithAddAddField (if add then 1 else 0) $
    insert regWithAddRegField reg 0

am2OffsetImmAddField :: Field
am2OffsetImmAddField = Field 1 12

am2OffsetImmImmField :: Field
am2OffsetImmImmField = Field 12 0

mkAm2OffsetImm :: Word32 -> Am2OffsetImm
mkAm2OffsetImm w = Am2OffsetImm (fromIntegral imm) (add == 1)
  where
    add = extract am2OffsetImmAddField w
    imm = extract am2OffsetImmImmField w

am2OffsetImmToBits :: Am2OffsetImm -> Word32
am2OffsetImmToBits (Am2OffsetImm imm add) =
    insert am2OffsetImmAddField (if add then 1 else 0) $
    insert am2OffsetImmImmField imm 0

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
                                (GPR $ fromIntegral reg) (add == 1)
  where
    add = extract am2OffsetRegAddField w
    imm = extract am2OffsetRegImmField w
    ty  = extract am2OffsetRegTypeField w
    reg = extract am2OffsetRegRegField w

am2OffsetRegToBits :: Am2OffsetReg -> Word32
am2OffsetRegToBits (Am2OffsetReg imm ty (GPR reg) add) =
    insert am2OffsetRegAddField (if add then 1 else 0) $
    insert am2OffsetRegImmField imm $
    insert am2OffsetRegTypeField ty $
    insert am2OffsetRegRegField reg 0

addrMode5AddField :: Field
addrMode5AddField = Field 1 8

addrMode5ImmField :: Field
addrMode5ImmField = Field 8 0

mkAddrMode5 :: Word32 -> AddrMode5
mkAddrMode5 w = AddrMode5 (fromIntegral imm) (add == 1)
  where
    add = extract addrMode5AddField w
    imm = extract addrMode5ImmField w

addrMode5ToBits :: AddrMode5 -> Word32
addrMode5ToBits (AddrMode5 imm add) =
    let allOne = Field 4 9
    in insert addrMode5AddField (if add then 1 else 0) $
       insert addrMode5ImmField imm $
       -- Always set bits 12:9 to 1 (see the tgen data and ARM ARM for LDCL
       -- etc. that use this operand type).
       insert allOne (2 ^ 5 - 1) 0

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
                           (fromIntegral imm)
                           (add == 1)
  where
    baseReg   = extract ldstSoRegBaseRegField w
    offsetReg = extract ldstSoRegOffsetRegField w
    add       = extract ldstSoRegAddField w
    imm       = extract ldstSoRegImmField w

ldstSoRegToBits :: LdstSoReg -> Word32
ldstSoRegToBits (LdstSoReg (GPR baseR) (GPR offsetR) imm add) =
    insert ldstSoRegBaseRegField baseR $
    insert ldstSoRegOffsetRegField offsetR $
    insert ldstSoRegAddField (if add then 1 else 0) $
    insert ldstSoRegImmField imm 0

imm12Field :: Field
imm12Field = Field 12 0

mkImm12 :: Word32 -> Imm12
mkImm12 w = Imm12 $ fromIntegral i
  where
    i = extract imm12Field w

imm12ToBits :: Imm12 -> Word32
imm12ToBits (Imm12 i) =
    insert imm12Field (fromIntegral i) 0

imm5Field :: Field
imm5Field = Field 5 0

mkImm5 :: Word32 -> Imm5
mkImm5 w = Imm5 $ fromIntegral i
  where
    i = extract imm5Field w

imm5ToBits :: Imm5 -> Word32
imm5ToBits (Imm5 i) =
    insert imm5Field (fromIntegral i) 0

mkCoprocRegister :: Word32 -> CoprocRegister
mkCoprocRegister w = CoprocRegister $ fromIntegral w

coprocRegisterToBits :: CoprocRegister -> Word32
coprocRegisterToBits (CoprocRegister i) = fromIntegral i

mkOpcode :: Word32 -> Opcode
mkOpcode w = Opcode $ fromIntegral w

opcodeToBits :: Opcode -> Word32
opcodeToBits (Opcode i) = fromIntegral i

branchTargetField :: Field
branchTargetField = Field 24 0

mkBranchTarget :: Word32 -> BranchTarget
mkBranchTarget w = BranchTarget $ fromIntegral i
  where
    i = extract branchTargetField w

branchTargetToBits :: BranchTarget -> Word32
branchTargetToBits (BranchTarget i) =
    insert branchTargetField (fromIntegral i) 0

branchExecuteTargetField1 :: Field
branchExecuteTargetField1 = Field 24 0

branchExecuteTargetField2 :: Field
branchExecuteTargetField2 = Field 1 24

mkBranchExecuteTarget :: Word32 -> BranchExecuteTarget
mkBranchExecuteTarget w = BranchExecuteTarget $ fromIntegral $ w `shiftL` 1

branchExecuteTargetToBits :: BranchExecuteTarget -> Word32
branchExecuteTargetToBits (BranchExecuteTarget i) =
    insert branchExecuteTargetField1 (i `shiftR` 1) 0

mkImm16 :: Word32 -> Imm16
mkImm16 = Imm16 . fromIntegral

imm16ToBits :: Imm16 -> Word32
imm16ToBits (Imm16 i) = fromIntegral i

mkSBit :: Word32 -> SBit
mkSBit = SBit . fromIntegral

sBitToBits :: SBit -> Word32
sBitToBits (SBit i) = fromIntegral i

mkPred :: Word32 -> Pred
mkPred = Pred . fromIntegral

predToBits :: Pred -> Word32
predToBits (Pred p) = fromIntegral p

addBitsField :: Field
addBitsField = Field 2 12

mkAdrLabel :: Word32 -> AdrLabel
mkAdrLabel w = AdrLabel i add
  where
    i = mkImm12 w
    addBits = extract addBitsField w
    add = case addBits of
        0b10 -> True
        0b1  -> False
        _    -> error $ "mkAdrLabel: unexpected addition bits: " <> show addBits

adrLabelToBits :: AdrLabel -> Word32
adrLabelToBits (AdrLabel imm add) =
    let addBits = if add
                  then 0b10
                  else 0b1
    in insert addBitsField addBits $
       imm12ToBits imm

mkMask :: Field -> Word32
mkMask (Field bits offset) = (2 ^ bits - 1) `shiftL` offset

insert :: (Integral a) => Field -> a -> Word32 -> Word32
insert (Field bits offset) src dest =
    dest .|. (((fromIntegral src) .&. (2 ^ bits - 1)) `shiftL` offset)

extract :: Field -> Word32 -> Word32
extract f val = (val .&. (mkMask f)) `shiftR` (fieldOffset f)

-- | An AddrMode3 memory reference for a load or store instruction
data AddrMode3 = AddrMode3 { addrMode3Register  :: GPR
                           , addrMode3Immediate :: Word8
                           , addrMode3Add       :: Bool
                           }
  deriving (Eq, Ord, Show)

-- | An am3Offset memory reference for a load or store instruction
data AM3Offset = AM3Offset { am3OffsetImmediate :: Word8
                           , am3OffsetAdd       :: Bool
                           }
  deriving (Eq, Ord, Show)

-- | A shift_imm operand with a shift immediate and shift type (l/r).
-- See also USAT in the ARM ARM and tgen.
data ShiftImm = ShiftImm { shiftImmImmediate :: Word8
                         , shiftImmType      :: Word8
                         }
  deriving (Eq, Ord, Show)

-- | An am2offset_reg operand with a register, immediate, and shift type
-- (l/r). See also LDRBT in the ARM ARM and tgen.
data Am2OffsetReg = Am2OffsetReg { am2OffsetRegImmediate :: Word8
                                 , am2OffsetRegType      :: Word8
                                 , am2OffsetRegReg       :: GPR
                                 , am2OffsetRegAdd       :: Bool
                                 }
  deriving (Eq, Ord, Show)

-- | An AddrMode5 memory reference
data AddrMode5 = AddrMode5 { addrMode5Immediate :: Word8
                           , addrMode5Add       :: Bool
                           }
  deriving (Eq, Ord, Show)

-- | An Am2offset_imm memory reference
data Am2OffsetImm = Am2OffsetImm { am2OffsetImmImmediate :: Word16
                                 , am2OffsetImmAdd       :: Bool
                                 }
  deriving (Eq, Ord, Show)

-- | An load/store memory reference for a preload (e.g. PLDW)
-- instruction
data LdstSoReg = LdstSoReg { ldstSoRegBaseRegister   :: GPR
                           , ldstSoRegOffsetRegister :: GPR
                           , ldstSoRegImmediate      :: Word8
                           , ldstSoRegAdd            :: Bool
                           }
  deriving (Eq, Ord, Show)

data RegWithAdd = RegWithAdd { regWithAddReg :: GPR
                             , regWithAddAdd :: Bool
                             }
  deriving (Eq, Ord, Show)

-- | An AddrMode_Imm12 memory reference for a load or store instruction
-- (with a 12-bit immediate)
data AddrModeImm12 = AddrModeImm12 { addrModeImm12Register  :: GPR
                                   , addrModeImm12Immediate :: Word8
                                   , addrModeImm12Add       :: Bool
                                   }
  deriving (Eq, Ord, Show)

-- | A twelve-bit immediate
data Imm12 = Imm12 { unImm12 :: Integer
                   }
  deriving (Eq, Ord, Show)

-- | A five-bit immediate
data Imm5 = Imm5 { unImm5 :: Word8
                 }
  deriving (Eq, Ord, Show)

-- | A branch target
data BranchTarget = BranchTarget { unBranchTarget :: Integer
                                 }
  deriving (Eq, Ord, Show)

-- | A branch-and-execute target
data BranchExecuteTarget = BranchExecuteTarget { unBranchExecuteTarget :: Integer
                                               }
  deriving (Eq, Ord, Show)

-- | A 16-bit immediate split into 12- and 4-bit chunks
data Imm16 = Imm16 { unImm16 :: Integer
                   }
  deriving (Eq, Ord, Show)

-- | A set-flags bit ('S' in the ARM ARM)
data SBit = SBit { unSBit :: Word8
                 }
  deriving (Eq, Ord, Show)

-- | An ADR immediate offset and addition bit
data AdrLabel = AdrLabel { adrLabelImm :: Imm12
                         , adrLabelAdd :: Bool
                         }
  deriving (Eq, Ord, Show)

-- | Four-bit condition flag sequence
data Pred = Pred { unPred :: Word8
                 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Bit where
  pPrint (Bit True) = PP.int 1
  pPrint (Bit False) = PP.int 0

instance PP.Pretty Imm16 where
  pPrint (Imm16 i) = PP.integer i

instance PP.Pretty CoprocRegister where
  pPrint (CoprocRegister r) = PP.char 'p' <> PP.pPrint r

instance PP.Pretty Opcode where
  pPrint (Opcode o) = PP.pPrint o

instance PP.Pretty GPR where
  pPrint (GPR rno) = PP.char 'r' <> PP.int (fromIntegral rno)

instance PP.Pretty DPR where
  pPrint (DPR rno) = PP.char 'd' <> PP.int (fromIntegral rno)

instance PP.Pretty QPR where
  pPrint (QPR rno) = PP.char 'q' <> PP.int (fromIntegral rno)

instance PP.Pretty QQPR where
  pPrint (QQPR rno) = PP.char 'q' <> PP.int (fromIntegral rno)

instance PP.Pretty Imm12 where
  pPrint = PP.pPrint . unImm12

instance PP.Pretty Imm5 where
  pPrint = PP.pPrint . unImm5

instance PP.Pretty BranchTarget where
  pPrint = PP.pPrint . unBranchTarget

instance PP.Pretty SBit where
  pPrint = PP.pPrint . unSBit

instance PP.Pretty Pred where
  pPrint = PP.pPrint . unPred

instance PP.Pretty AdrLabel where
  pPrint (AdrLabel imm add) =
      let opStr = if add then mempty else PP.char '-'
      in opStr <> PP.pPrint imm

instance PP.Pretty AddrMode3 where
  pPrint m =
      let opStr = if addrMode3Add m then mempty else PP.char '-'
      in (PP.pPrint (addrMode3Register m) <> PP.char ',') PP.<+>
         (opStr <> PP.pPrint (addrMode3Immediate m))

instance PP.Pretty Am2OffsetImm where
  pPrint m =
      let opStr = if am2OffsetImmAdd m then mempty else PP.char '-'
      in (opStr <> PP.pPrint (am2OffsetImmImmediate m))

instance PP.Pretty Am2OffsetReg where
  pPrint m =
      let tyStr = if am2OffsetRegType m == 1 then "ASR" else "LSL"
          -- See the ARM ARM on USAT for information on this
          -- representation.
          amtStr = if am2OffsetRegType m == 0
                   then show $ am2OffsetRegImmediate m
                   else if am2OffsetRegImmediate m == 0
                        then "32"
                        else show $ am2OffsetRegImmediate m
          opStr = if am2OffsetRegAdd m then mempty else PP.char '-'
      in (PP.pPrint (am2OffsetRegReg m) <> PP.char ',') PP.<+>
         (PP.text tyStr PP.<+> PP.text amtStr)

instance PP.Pretty AddrMode5 where
  pPrint m =
      let opStr = if addrMode5Add m then mempty else PP.char '-'
      in (opStr <> PP.pPrint (addrMode5Immediate m))

instance PP.Pretty ShiftImm where
  pPrint m =
      let tyStr = if shiftImmType m == 1 then "ASR" else "LSL"
          -- See the ARM ARM on USAT for information on this
          -- representation.
          amtStr = if shiftImmType m == 0
                   then show $ shiftImmImmediate m
                   else if shiftImmImmediate m == 0
                        then "32"
                        else show $ shiftImmImmediate m
      in (PP.text tyStr PP.<+> PP.text amtStr)

instance PP.Pretty BranchExecuteTarget where
    pPrint (BranchExecuteTarget t) = PP.pPrint t

instance PP.Pretty LdstSoReg where
  pPrint m =
      let opStr = if ldstSoRegAdd m then mempty else PP.char '-'
      in (PP.pPrint (ldstSoRegBaseRegister m) <> PP.char ',') PP.<+>
         (opStr <> ((PP.pPrint (ldstSoRegOffsetRegister m) <> PP.char ',') PP.<+>
                    (PP.pPrint (ldstSoRegImmediate m))))

instance PP.Pretty AM3Offset where
  pPrint m =
      let opStr = if am3OffsetAdd m then mempty else PP.char '-'
      in (opStr <> PP.pPrint (am3OffsetImmediate m))

instance PP.Pretty RegWithAdd where
  pPrint m =
      let opStr = if regWithAddAdd m then mempty else PP.char '-'
      in (opStr <> PP.pPrint (regWithAddReg m))

instance PP.Pretty AddrModeImm12 where
  pPrint m =
      let opStr = if addrModeImm12Add m then mempty else PP.char '-'
      in (PP.pPrint (addrModeImm12Register m) <> PP.char ',') PP.<+>
         (opStr <> PP.pPrint (addrModeImm12Immediate m))
