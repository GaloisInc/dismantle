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

  AddrMode3(..),
  mkAddrMode3,
  addrMode3ToBits,

  AddrOffsetNone(..),
  mkAddrOffsetNone,
  addrOffsetNoneToBits,

  BranchTarget(..),
  mkBranchTarget,
  branchTargetToBits,

  BranchExecuteTarget(..),
  mkBranchExecuteTarget,
  branchExecuteTargetToBits,

  Imm12(..),
  mkImm12,
  imm12ToBits,

  Imm12_4(..),
  mkImm12_4,
  imm12_4ToBits,

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
addrMode3RegField = Field 4 16

addrMode3AddField :: Field
addrMode3AddField = Field 1 23

addrMode3ImmLField :: Field
addrMode3ImmLField = Field 4 0

addrMode3ImmHField :: Field
addrMode3ImmHField = Field 4 8

mkAddrMode3 :: Word32 -> AddrMode3
mkAddrMode3 w = AddrMode3 (GPR $ fromIntegral reg) (fromIntegral imm) (add == 1)
  where
    reg   = extract addrMode3RegField w
    add   = extract addrMode3AddField w
    imm4L = extract addrMode3ImmLField w
    imm4H = extract addrMode3ImmHField w
    imm   = imm4L .|. (imm4H `shiftL` 4)

addrMode3ToBits :: AddrMode3 -> Word32
addrMode3ToBits (AddrMode3 (GPR r) imm add) =
    insert addrMode3RegField r $
    insert addrMode3AddField (if add then 1 else 0) $
    insert addrMode3ImmLField imm $
    insert addrMode3ImmHField (imm `shiftR` 4) 0

imm12Field :: Field
imm12Field = Field 12 0

mkImm12 :: Word32 -> Imm12
mkImm12 w = Imm12 $ fromIntegral i
  where
    i = extract imm12Field w

imm12ToBits :: Imm12 -> Word32
imm12ToBits (Imm12 i) =
    insert imm12Field (fromIntegral i) 0

mkCoprocRegister :: Word32 -> CoprocRegister
mkCoprocRegister w = CoprocRegister $ fromIntegral w

coprocRegisterToBits :: CoprocRegister -> Word32
coprocRegisterToBits (CoprocRegister i) = fromIntegral i

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
mkBranchExecuteTarget w = BranchExecuteTarget $ fromIntegral t
  where
    hi = extract branchExecuteTargetField1 w
    lo = extract branchExecuteTargetField2 w
    t = hi `shiftL` 2 .|.
        lo `shiftL` 1

branchExecuteTargetToBits :: BranchExecuteTarget -> Word32
branchExecuteTargetToBits (BranchExecuteTarget i) =
    insert branchExecuteTargetField1 (fromIntegral $ i `shiftR` 2) $
    insert branchExecuteTargetField2 (fromIntegral $ i `shiftR` 1) 0

imm12_4Field1 :: Field
imm12_4Field1 = Field 4 0

imm12_4Field2 :: Field
imm12_4Field2 = Field 12 8

mkImm12_4 :: Word32 -> Imm12_4
mkImm12_4 w = Imm12_4 $ fromIntegral i
  where
    hi = extract imm12_4Field2 w
    lo = extract imm12_4Field1 w
    i = (hi `shiftL` 4) .|. lo

imm12_4ToBits :: Imm12_4 -> Word32
imm12_4ToBits (Imm12_4 i) =
    insert imm12_4Field1 (fromIntegral i) $
    insert imm12_4Field2 (fromIntegral $ i `shiftR` 4) 0

sBitField :: Field
sBitField = Field 1 20

mkSBit :: Word32 -> SBit
mkSBit w = SBit $ fromIntegral i
  where
    i = extract sBitField w

sBitToBits :: SBit -> Word32
sBitToBits (SBit i) =
    insert sBitField (fromIntegral i) 0

predField :: Field
predField = Field 4 28

mkPred :: Word32 -> Pred
mkPred w = Pred $ fromIntegral i
  where
    i = extract predField w

predToBits :: Pred -> Word32
predToBits (Pred p) =
    insert predField (fromIntegral p) 0

addBitsField :: Field
addBitsField = Field 2 22

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

mkAddrOffsetNone :: Word32 -> AddrOffsetNone
mkAddrOffsetNone w = AddrOffsetNone (GPR $ fromIntegral reg)
  where
    reg = extract addrMode3RegField w

addrOffsetNoneToBits :: AddrOffsetNone -> Word32
addrOffsetNoneToBits (AddrOffsetNone (GPR r)) =
    insert addrMode3RegField r 0

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

-- | An addressing mode with no offset
data AddrOffsetNone = AddrOffsetNone { addrOffsetNoneRegister :: GPR
                                     }
  deriving (Eq, Ord, Show)

-- | A twelve-bit immediate
data Imm12 = Imm12 { unImm12 :: Integer
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
data Imm12_4 = Imm12_4 { unImm12_4 :: Integer
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

instance PP.Pretty AddrOffsetNone where
  pPrint = PP.pPrint . addrOffsetNoneRegister

instance PP.Pretty Imm12 where
  pPrint = PP.pPrint . unImm12

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
