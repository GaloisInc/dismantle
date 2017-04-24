{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE BinaryLiterals #-}
module Dismantle.ARM.Operands (
  GPR(..),
  DPR(..),
  QPR(..),
  QQPR(..),
  CR(..),
  FR(..),
  VR(..),

  CoprocRegister(..),
  mkCoprocRegister,
  coprocRegisterToBits,

  Opcode(..),
  mkOpcode,
  opcodeToBits,

  AddrMode3(..),
  mkAddrMode3,
  addrMode3ToBits,

  BranchTarget(..),
  mkBranchTarget,
  branchTargetToBits,

  BranchExecuteTarget(..),
  mkBranchExecuteTarget,
  branchExecuteTargetToBits,

  Imm12(..),
  mkImm12,
  imm12ToBits,

  Imm5(..),
  mkImm5,
  imm5ToBits,

  Imm16(..),
  mkImm16,
  imm16ToBits,

  SBit(..),
  mkSBit,
  sBitToBits,

  AdrLabel(..),
  mkAdrLabel,
  adrLabelToBits,

  Pred(..),
  mkPred,
  predToBits
  ) where

import Data.Bits
import Data.Monoid
import Data.Word ( Word8, Word16, Word32 )

import qualified Text.PrettyPrint.HughesPJClass as PP

import Dismantle.Tablegen.TH.Pretty

newtype CR = CR { unCR :: Word8 }
  deriving (Eq, Ord, Show)

-- | Floating-point register by number
newtype FR = FR { unFR :: Word8 }
  deriving (Eq, Ord, Show)

-- | General-purpose register by number
newtype GPR = GPR { unGPR :: Word8 }
  deriving (Eq, Ord, Show)

-- | Coprocessor register by number
newtype CoprocRegister = CoprocRegister { unCoprocRegister :: Word8 }
  deriving (Eq, Ord, Show)

-- | Coprocessor operation opcode register by number
newtype Opcode = Opcode { unOpcode :: Word8 }
  deriving (Eq, Ord, Show)

-- | Double-precision register by number
newtype DPR = DPR { unDPR :: Word8 }
  deriving (Eq, Ord, Show)

-- | 128-bit vector register by number
newtype QPR = QPR { unQPR :: Word8 }
  deriving (Eq, Ord, Show)

-- | 256-bit vector register (128-bit register pair) by number (must be
-- even)
newtype QQPR = QQPR { unQQPR :: Word8 }
  deriving (Eq, Ord, Show)

-- | Vector register by number
newtype VR = VR { unVR :: Word8 }
  deriving (Eq, Ord, Show)

data Field = Field { fieldBits :: Int
                   , fieldOffset :: Int
                   }

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

instance PP.Pretty GPR where
  pPrint (GPR rno) = PP.char 'r' <> PP.int (fromIntegral rno)

instance PP.Pretty DPR where
  pPrint (DPR rno) = PP.char 'd' <> PP.int (fromIntegral rno)

instance PP.Pretty QPR where
  pPrint (QPR rno) = PP.char 'q' <> PP.int (fromIntegral rno)

instance PP.Pretty QQPR where
  pPrint (QQPR rno) = PP.char 'q' <> PP.int (fromIntegral rno)

instance PP.Pretty CR where
  pPrint (CR rno) = PP.char 'c' <> PP.int (fromIntegral rno)

instance PP.Pretty FR where
  pPrint (FR rno) = PP.char 'f' <> PP.int (fromIntegral rno)

instance PP.Pretty VR where
  pPrint (VR rno) = PP.char 'v' <> PP.int (fromIntegral rno)

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
