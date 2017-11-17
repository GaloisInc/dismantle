{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
module Dismantle.AArch64.Operands
  ( FPR128
  , mkFPR128
  , fPR128ToBits
  , fPR128Operand

  , FPR16
  , mkFPR16
  , fPR16ToBits
  , fPR16Operand

  , FPR32
  , mkFPR32
  , fPR32ToBits
  , fPR32Operand

  , FPR64
  , mkFPR64
  , fPR64ToBits
  , fPR64Operand

  , FPR8
  , mkFPR8
  , fPR8ToBits
  , fPR8Operand

  , GPR32
  , mkGPR32
  , gPR32ToBits
  , gPR32Operand

  , GPR64
  , mkGPR64
  , gPR64ToBits
  , gPR64Operand

  , GPR32sp
  , mkGPR32sp
  , gPR32spToBits
  , gPR32spOperand

  , GPR64sp
  , mkGPR64sp
  , gPR64spToBits
  , gPR64spOperand

  , V128
  , mkV128
  , v128ToBits
  , v128Operand

  , AddsubShiftedImm32
  , mkAddsubShiftedImm32
  , addsubShiftedImm32ToBits
  , addsubShiftedImm32Operand

  , AddsubShiftedImm64
  , mkAddsubShiftedImm64
  , addsubShiftedImm64ToBits
  , addsubShiftedImm64Operand

  , Adrlabel
  , mkAdrlabel
  , adrlabelToBits
  , adrlabelOperand

  , Adrplabel
  , mkAdrplabel
  , adrplabelToBits
  , adrplabelOperand

  , AmBTarget
  , mkAmBTarget
  , amBTargetToBits
  , amBTargetOperand

  , AmBrcond
  , mkAmBrcond
  , amBrcondToBits
  , amBrcondOperand

  , AmLdrlit
  , mkAmLdrlit
  , amLdrlitToBits
  , amLdrlitOperand

  , AmTbrcond
  , mkAmTbrcond
  , amTbrcondToBits
  , amTbrcondOperand

  )
where

import Data.Bits
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Word ( Word8, Word16, Word32 )
import Data.Int ( Int16, Int32 )

import Numeric (showHex)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Text.PrettyPrint.HughesPJClass as PP

import Dismantle.Tablegen.ISA
import qualified Dismantle.Arbitrary as A
import Dismantle.Tablegen.TH.Pretty

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

-- Operand types

data FPR128 = FPR128 { fPR128Reg :: Word8
                     } deriving (Eq, Ord, Show)

instance PP.Pretty FPR128 where
  pPrint (FPR128 r) = PP.text $ "Q" <> show r

instance A.Arbitrary FPR128 where
  arbitrary g = FPR128 <$> A.arbitrary g

fPR128RegField :: Field
fPR128RegField = Field 5 0

fPR128ToBits :: FPR128 -> Word32
fPR128ToBits val =
  insert fPR128RegField (fPR128Reg val) 0

mkFPR128 :: Word32 -> FPR128
mkFPR128 w =
  FPR128 (fromIntegral $ extract fPR128RegField w)

fPR128Operand :: OperandPayload
fPR128Operand =
  OperandPayload { opTypeT = [t| FPR128 |]
                 , opConE  = Just (varE 'mkFPR128)
                 , opWordE = Just (varE 'fPR128ToBits)
                 }

data FPR16 = FPR16 { fPR16Reg :: Word8
                     } deriving (Eq, Ord, Show)

instance PP.Pretty FPR16 where
  pPrint (FPR16 r) = PP.text $ "H" <> show r

instance A.Arbitrary FPR16 where
  arbitrary g = FPR16 <$> A.arbitrary g

fPR16RegField :: Field
fPR16RegField = Field 5 0

fPR16ToBits :: FPR16 -> Word32
fPR16ToBits val =
  insert fPR16RegField (fPR16Reg val) 0

mkFPR16 :: Word32 -> FPR16
mkFPR16 w =
  FPR16 (fromIntegral $ extract fPR16RegField w)

fPR16Operand :: OperandPayload
fPR16Operand =
  OperandPayload { opTypeT = [t| FPR16 |]
                 , opConE  = Just (varE 'mkFPR16)
                 , opWordE = Just (varE 'fPR16ToBits)
                 }

data FPR32 = FPR32 { fPR32Reg :: Word8
                     } deriving (Eq, Ord, Show)

instance PP.Pretty FPR32 where
  pPrint (FPR32 r) = PP.text $ "S" <> show r

instance A.Arbitrary FPR32 where
  arbitrary g = FPR32 <$> A.arbitrary g

fPR32RegField :: Field
fPR32RegField = Field 5 0

fPR32ToBits :: FPR32 -> Word32
fPR32ToBits val =
  insert fPR32RegField (fPR32Reg val) 0

mkFPR32 :: Word32 -> FPR32
mkFPR32 w =
  FPR32 (fromIntegral $ extract fPR32RegField w)

fPR32Operand :: OperandPayload
fPR32Operand =
  OperandPayload { opTypeT = [t| FPR32 |]
                 , opConE  = Just (varE 'mkFPR32)
                 , opWordE = Just (varE 'fPR32ToBits)
                 }

data FPR64 = FPR64 { fPR64Reg :: Word8
                   } deriving (Eq, Ord, Show)

instance PP.Pretty FPR64 where
  pPrint (FPR64 r) = PP.text $ "D" <> show r

instance A.Arbitrary FPR64 where
  arbitrary g = FPR64 <$> A.arbitrary g

fPR64RegField :: Field
fPR64RegField = Field 5 0

fPR64ToBits :: FPR64 -> Word32
fPR64ToBits val =
  insert fPR64RegField (fPR64Reg val) 0

mkFPR64 :: Word32 -> FPR64
mkFPR64 w =
  FPR64 (fromIntegral $ extract fPR64RegField w)

fPR64Operand :: OperandPayload
fPR64Operand =
  OperandPayload { opTypeT = [t| FPR64 |]
                 , opConE  = Just (varE 'mkFPR64)
                 , opWordE = Just (varE 'fPR64ToBits)
                 }

data FPR8 = FPR8 { fPR8Reg :: Word8
                 } deriving (Eq, Ord, Show)

instance PP.Pretty FPR8 where
  pPrint (FPR8 r) = PP.text $ "B" <> show r

instance A.Arbitrary FPR8 where
  arbitrary g = FPR8 <$> A.arbitrary g

fPR8RegField :: Field
fPR8RegField = Field 5 0

fPR8ToBits :: FPR8 -> Word32
fPR8ToBits val =
  insert fPR8RegField (fPR8Reg val) 0

mkFPR8 :: Word32 -> FPR8
mkFPR8 w =
  FPR8 (fromIntegral $ extract fPR8RegField w)

fPR8Operand :: OperandPayload
fPR8Operand =
  OperandPayload { opTypeT = [t| FPR8 |]
                 , opConE  = Just (varE 'mkFPR8)
                 , opWordE = Just (varE 'fPR8ToBits)
                 }

data GPR32 = GPR32 { gPR32Reg :: Word8
                   } deriving (Eq, Ord, Show)

instance PP.Pretty GPR32 where
  pPrint (GPR32 r) = PP.text $ "W" <> show r

instance A.Arbitrary GPR32 where
  arbitrary g = GPR32 <$> A.arbitrary g

gPR32RegField :: Field
gPR32RegField = Field 5 0

gPR32ToBits :: GPR32 -> Word32
gPR32ToBits val =
  insert gPR32RegField (gPR32Reg val) 0

mkGPR32 :: Word32 -> GPR32
mkGPR32 w =
  GPR32 (fromIntegral $ extract gPR32RegField w)

gPR32Operand :: OperandPayload
gPR32Operand =
  OperandPayload { opTypeT = [t| GPR32 |]
                 , opConE  = Just (varE 'mkGPR32)
                 , opWordE = Just (varE 'gPR32ToBits)
                 }

data GPR64 = GPR64 { gPR64Reg :: Word8
                   } deriving (Eq, Ord, Show)

instance PP.Pretty GPR64 where
  pPrint (GPR64 r) = PP.text $ "X" <> show r

instance A.Arbitrary GPR64 where
  arbitrary g = GPR64 <$> A.arbitrary g

gPR64RegField :: Field
gPR64RegField = Field 5 0

gPR64ToBits :: GPR64 -> Word32
gPR64ToBits val =
  insert gPR64RegField (gPR64Reg val) 0

mkGPR64 :: Word32 -> GPR64
mkGPR64 w =
  GPR64 (fromIntegral $ extract gPR64RegField w)

gPR64Operand :: OperandPayload
gPR64Operand =
  OperandPayload { opTypeT = [t| GPR64 |]
                 , opConE  = Just (varE 'mkGPR64)
                 , opWordE = Just (varE 'gPR64ToBits)
                 }

data GPR32sp = GPR32sp { gPR32spReg :: Word8
                       } deriving (Eq, Ord, Show)

instance PP.Pretty GPR32sp where
  pPrint (GPR32sp 0b11111) = PP.text "WSP"
  pPrint (GPR32sp r) = PP.text $ "W" <> show r

instance A.Arbitrary GPR32sp where
  arbitrary g = GPR32sp <$> A.arbitrary g

gPR32spRegField :: Field
gPR32spRegField = Field 5 0

gPR32spToBits :: GPR32sp -> Word32
gPR32spToBits val =
  insert gPR32spRegField (gPR32spReg val) 0

mkGPR32sp :: Word32 -> GPR32sp
mkGPR32sp w =
  GPR32sp (fromIntegral $ extract gPR32spRegField w)

gPR32spOperand :: OperandPayload
gPR32spOperand =
  OperandPayload { opTypeT = [t| GPR32sp |]
                 , opConE  = Just (varE 'mkGPR32sp)
                 , opWordE = Just (varE 'gPR32spToBits)
                 }

data GPR64sp = GPR64sp { gPR64spReg :: Word8
                       } deriving (Eq, Ord, Show)

instance PP.Pretty GPR64sp where
  pPrint (GPR64sp 0b11111) = PP.text "XSP"
  pPrint (GPR64sp r) = PP.text $ "X" <> show r

instance A.Arbitrary GPR64sp where
  arbitrary g = GPR64sp <$> A.arbitrary g

gPR64spRegField :: Field
gPR64spRegField = Field 5 0

gPR64spToBits :: GPR64sp -> Word32
gPR64spToBits val =
  insert gPR64spRegField (gPR64spReg val) 0

mkGPR64sp :: Word32 -> GPR64sp
mkGPR64sp w =
  GPR64sp $ (fromIntegral $ extract gPR64spRegField w)

gPR64spOperand :: OperandPayload
gPR64spOperand =
  OperandPayload { opTypeT = [t| GPR64sp |]
                 , opConE  = Just (varE 'mkGPR64sp)
                 , opWordE = Just (varE 'gPR64spToBits)
                 }

data V128 = V128 { v128Reg :: Word8
                 } deriving (Eq, Ord, Show)

instance PP.Pretty V128 where
  pPrint (V128 r) = PP.text $ "V" <> show r

instance A.Arbitrary V128 where
  arbitrary g = V128 <$> A.arbitrary g

v128RegField :: Field
v128RegField = Field 5 0

v128ToBits :: V128 -> Word32
v128ToBits val =
  insert v128RegField (v128Reg val) 0

mkV128 :: Word32 -> V128
mkV128 w =
  V128 $ (fromIntegral $ extract v128RegField w)

v128Operand :: OperandPayload
v128Operand =
  OperandPayload { opTypeT = [t| V128 |]
                 , opConE  = Just (varE 'mkV128)
                 , opWordE = Just (varE 'v128ToBits)
                 }

data AddsubShiftedImm32 = AddsubShiftedImm32 { addsubShiftedImm32Imm :: Word16
                                             , addsubShiftedImm32Shift :: Word8
                                             } deriving (Eq, Ord, Show)

instance PP.Pretty AddsubShiftedImm32 where
  pPrint _ = PP.text "AddsubShiftedImm32: not implemented"

instance A.Arbitrary AddsubShiftedImm32 where
  arbitrary g = AddsubShiftedImm32 <$> A.arbitrary g <*> A.arbitrary g

addsubShiftedImm32ImmField :: Field
addsubShiftedImm32ImmField = Field 12 0

addsubShiftedImm32ShiftField :: Field
addsubShiftedImm32ShiftField = Field 2 12

addsubShiftedImm32ToBits :: AddsubShiftedImm32 -> Word32
addsubShiftedImm32ToBits val =
  insert addsubShiftedImm32ImmField (addsubShiftedImm32Imm val) $
  insert addsubShiftedImm32ShiftField (addsubShiftedImm32Shift val) 0

mkAddsubShiftedImm32 :: Word32 -> AddsubShiftedImm32
mkAddsubShiftedImm32 w =
  AddsubShiftedImm32 (fromIntegral $ extract addsubShiftedImm32ImmField w)
                     (fromIntegral $ extract addsubShiftedImm32ShiftField w)

addsubShiftedImm32Operand :: OperandPayload
addsubShiftedImm32Operand =
  OperandPayload { opTypeT = [t| AddsubShiftedImm32 |]
                 , opConE  = Just (varE 'mkAddsubShiftedImm32)
                 , opWordE = Just (varE 'addsubShiftedImm32ToBits)
                 }

data AddsubShiftedImm64 = AddsubShiftedImm64 { addsubShiftedImm64Imm :: Word16
                                             , addsubShiftedImm64Shift :: Word8
                                             } deriving (Eq, Ord, Show)

instance PP.Pretty AddsubShiftedImm64 where
  pPrint _ = PP.text "AddsubShiftedImm64: not implemented"

instance A.Arbitrary AddsubShiftedImm64 where
  arbitrary g = AddsubShiftedImm64 <$> A.arbitrary g <*> A.arbitrary g

addsubShiftedImm64ImmField :: Field
addsubShiftedImm64ImmField = Field 12 0

addsubShiftedImm64ShiftField :: Field
addsubShiftedImm64ShiftField = Field 2 12

addsubShiftedImm64ToBits :: AddsubShiftedImm64 -> Word32
addsubShiftedImm64ToBits val =
  insert addsubShiftedImm64ImmField (addsubShiftedImm64Imm val) $
  insert addsubShiftedImm64ShiftField (addsubShiftedImm64Shift val) 0

mkAddsubShiftedImm64 :: Word32 -> AddsubShiftedImm64
mkAddsubShiftedImm64 w =
  AddsubShiftedImm64 (fromIntegral $ extract addsubShiftedImm64ImmField w)
                     (fromIntegral $ extract addsubShiftedImm64ShiftField w)

addsubShiftedImm64Operand :: OperandPayload
addsubShiftedImm64Operand =
  OperandPayload { opTypeT = [t| AddsubShiftedImm64 |]
                 , opConE  = Just (varE 'mkAddsubShiftedImm64)
                 , opWordE = Just (varE 'addsubShiftedImm64ToBits)
                 }

data Adrlabel = Adrlabel { adrlabelImm :: Word32
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Adrlabel where
  pPrint _ = PP.text "Adrlabel: not implemented"

instance A.Arbitrary Adrlabel where
  arbitrary g = Adrlabel <$> A.arbitrary g

adrlabelImmField :: Field
adrlabelImmField = Field 21 0

adrlabelToBits :: Adrlabel -> Word32
adrlabelToBits val =
  insert adrlabelImmField (adrlabelImm val) 0

mkAdrlabel :: Word32 -> Adrlabel
mkAdrlabel w =
  Adrlabel (fromIntegral $ extract adrlabelImmField w)

adrlabelOperand :: OperandPayload
adrlabelOperand =
  OperandPayload { opTypeT = [t| Adrlabel |]
                 , opConE  = Just (varE 'mkAdrlabel)
                 , opWordE = Just (varE 'adrlabelToBits)
                 }

data Adrplabel = Adrplabel { adrplabelImm :: Word32
                           } deriving (Eq, Ord, Show)

instance PP.Pretty Adrplabel where
  pPrint _ = PP.text "Adrplabel: not implemented"

instance A.Arbitrary Adrplabel where
  arbitrary g = Adrplabel <$> A.arbitrary g

adrplabelImmField :: Field
adrplabelImmField = Field 21 0

adrplabelToBits :: Adrplabel -> Word32
adrplabelToBits val =
  insert adrplabelImmField (adrplabelImm val) 0

mkAdrplabel :: Word32 -> Adrplabel
mkAdrplabel w =
  Adrplabel (fromIntegral $ extract adrplabelImmField w)

adrplabelOperand :: OperandPayload
adrplabelOperand =
  OperandPayload { opTypeT = [t| Adrplabel |]
                 , opConE  = Just (varE 'mkAdrplabel)
                 , opWordE = Just (varE 'adrplabelToBits)
                 }

data AmBTarget = AmBTarget { amBTargetAddr :: Word32
                           } deriving (Eq, Ord, Show)

instance PP.Pretty AmBTarget where
  pPrint _ = PP.text "AmBTarget: not implemented"

instance A.Arbitrary AmBTarget where
  arbitrary g = AmBTarget <$> A.arbitrary g

amBTargetAddrField :: Field
amBTargetAddrField = Field 26 0

amBTargetToBits :: AmBTarget -> Word32
amBTargetToBits val =
  insert amBTargetAddrField (amBTargetAddr val) 0

mkAmBTarget :: Word32 -> AmBTarget
mkAmBTarget w =
  AmBTarget (fromIntegral $ extract amBTargetAddrField w)

amBTargetOperand :: OperandPayload
amBTargetOperand =
  OperandPayload { opTypeT = [t| AmBTarget |]
                 , opConE  = Just (varE 'mkAmBTarget)
                 , opWordE = Just (varE 'amBTargetToBits)
                 }

data AmBrcond = AmBrcond { amBrcondAddr :: Word32
                         } deriving (Eq, Ord, Show)

instance PP.Pretty AmBrcond where
  pPrint _ = PP.text "AmBrcond: not implemented"

instance A.Arbitrary AmBrcond where
  arbitrary g = AmBrcond <$> A.arbitrary g

amBrcondAddrField :: Field
amBrcondAddrField = Field 19 0

amBrcondToBits :: AmBrcond -> Word32
amBrcondToBits val =
  insert amBrcondAddrField (amBrcondAddr val) 0

mkAmBrcond :: Word32 -> AmBrcond
mkAmBrcond w =
  AmBrcond (fromIntegral $ extract amBrcondAddrField w)

amBrcondOperand :: OperandPayload
amBrcondOperand =
  OperandPayload { opTypeT = [t| AmBrcond |]
                 , opConE  = Just (varE 'mkAmBrcond)
                 , opWordE = Just (varE 'amBrcondToBits)
                 }

data AmLdrlit = AmLdrlit { amLdrlitLabel :: Word32
                         } deriving (Eq, Ord, Show)

instance PP.Pretty AmLdrlit where
  pPrint _ = PP.text "AmLdrlit: not implemented"

instance A.Arbitrary AmLdrlit where
  arbitrary g = AmLdrlit <$> A.arbitrary g

amLdrlitLabelField :: Field
amLdrlitLabelField = Field 19 0

amLdrlitToBits :: AmLdrlit -> Word32
amLdrlitToBits val =
  insert amLdrlitLabelField (amLdrlitLabel val) 0

mkAmLdrlit :: Word32 -> AmLdrlit
mkAmLdrlit w =
  AmLdrlit (fromIntegral $ extract amLdrlitLabelField w)

amLdrlitOperand :: OperandPayload
amLdrlitOperand =
  OperandPayload { opTypeT = [t| AmLdrlit |]
                 , opConE  = Just (varE 'mkAmLdrlit)
                 , opWordE = Just (varE 'amLdrlitToBits)
                 }

data AmTbrcond = AmTbrcond { amTbrcondLabel :: Word16
                           } deriving (Eq, Ord, Show)

instance PP.Pretty AmTbrcond where
  pPrint _ = PP.text "AmTbrcond: not implemented"

instance A.Arbitrary AmTbrcond where
  arbitrary g = AmTbrcond <$> A.arbitrary g

amTbrcondLabelField :: Field
amTbrcondLabelField = Field 14 0

amTbrcondToBits :: AmTbrcond -> Word32
amTbrcondToBits val =
  insert amTbrcondLabelField (amTbrcondLabel val) 0

mkAmTbrcond :: Word32 -> AmTbrcond
mkAmTbrcond w =
  AmTbrcond (fromIntegral $ extract amTbrcondLabelField w)

amTbrcondOperand :: OperandPayload
amTbrcondOperand =
  OperandPayload { opTypeT = [t| AmTbrcond |]
                 , opConE  = Just (varE 'mkAmTbrcond)
                 , opWordE = Just (varE 'amTbrcondToBits)
                 }

