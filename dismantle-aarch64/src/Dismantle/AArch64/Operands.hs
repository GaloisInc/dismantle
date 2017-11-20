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

  , ArithExtendlsl64
  , mkArithExtendlsl64
  , arithExtendlsl64ToBits
  , arithExtendlsl64Operand

  , BarrierOp
  , mkBarrierOp
  , barrierOpToBits
  , barrierOpOperand

  , Imm01
  , mkImm01
  , imm01ToBits
  , imm01Operand

  , Imm0127
  , mkImm0127
  , imm0127ToBits
  , imm0127Operand

  , Imm015
  , mkImm015
  , imm015ToBits
  , imm015Operand

  , Imm031
  , mkImm031
  , imm031ToBits
  , imm031Operand

  , Imm063
  , mkImm063
  , imm063ToBits
  , imm063Operand

  , Imm065535
  , mkImm065535
  , imm065535ToBits
  , imm065535Operand

  , Imm07
  , mkImm07
  , imm07ToBits
  , imm07Operand

  , Imm32015
  , mkImm32015
  , imm32015ToBits
  , imm32015Operand

  , Imm32031
  , mkImm32031
  , imm32031ToBits
  , imm32031Operand

  , LogicalImm32
  , mkLogicalImm32
  , logicalImm32ToBits
  , logicalImm32Operand

  , LogicalImm64
  , mkLogicalImm64
  , logicalImm64ToBits
  , logicalImm64Operand

  , Movimm32Imm
  , mkMovimm32Imm
  , movimm32ImmToBits
  , movimm32ImmOperand

  , Movimm32Shift
  , mkMovimm32Shift
  , movimm32ShiftToBits
  , movimm32ShiftOperand

  , Movimm64Shift
  , mkMovimm64Shift
  , movimm64ShiftToBits
  , movimm64ShiftOperand

  , MrsSysregOp
  , mkMrsSysregOp
  , mrsSysregOpToBits
  , mrsSysregOpOperand

  , MsrSysregOp
  , mkMsrSysregOp
  , msrSysregOpToBits
  , msrSysregOpOperand

  , Prfop
  , mkPrfop
  , prfopToBits
  , prfopOperand

  , Pstatefield1Op
  , mkPstatefield1Op
  , pstatefield1OpToBits
  , pstatefield1OpOperand

  , Pstatefield4Op
  , mkPstatefield4Op
  , pstatefield4OpToBits
  , pstatefield4OpOperand

  , RoWextend128
  , mkRoWextend128
  , roWextend128ToBits
  , roWextend128Operand

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

data ArithExtendlsl64 = ArithExtendlsl64 { arithExtendlsl64Shift :: Word8
                                         } deriving (Eq, Ord, Show)

instance PP.Pretty ArithExtendlsl64 where
  pPrint _ = PP.text "ArithExtendlsl64: not implemented"

instance A.Arbitrary ArithExtendlsl64 where
  arbitrary g = ArithExtendlsl64 <$> A.arbitrary g

arithExtendlsl64ShiftField :: Field
arithExtendlsl64ShiftField = Field 3 0

arithExtendlsl64ToBits :: ArithExtendlsl64 -> Word32
arithExtendlsl64ToBits val =
  insert arithExtendlsl64ShiftField (arithExtendlsl64Shift val) 0

mkArithExtendlsl64 :: Word32 -> ArithExtendlsl64
mkArithExtendlsl64 w =
  ArithExtendlsl64 (fromIntegral $ extract arithExtendlsl64ShiftField w)

arithExtendlsl64Operand :: OperandPayload
arithExtendlsl64Operand =
  OperandPayload { opTypeT = [t| ArithExtendlsl64 |]
                 , opConE  = Just (varE 'mkArithExtendlsl64)
                 , opWordE = Just (varE 'arithExtendlsl64ToBits)
                 }

data BarrierOp = BarrierOp { barrierOpType :: Word8
                           } deriving (Eq, Ord, Show)

instance PP.Pretty BarrierOp where
  pPrint _ = PP.text "BarrierOp: not implemented"

instance A.Arbitrary BarrierOp where
  arbitrary g = BarrierOp <$> A.arbitrary g

barrierOpTypeField :: Field
barrierOpTypeField = Field 4 0

barrierOpToBits :: BarrierOp -> Word32
barrierOpToBits val =
  insert barrierOpTypeField (barrierOpType val) 0

mkBarrierOp :: Word32 -> BarrierOp
mkBarrierOp w =
  BarrierOp (fromIntegral $ extract barrierOpTypeField w)

barrierOpOperand :: OperandPayload
barrierOpOperand =
  OperandPayload { opTypeT = [t| BarrierOp |]
                 , opConE  = Just (varE 'mkBarrierOp)
                 , opWordE = Just (varE 'barrierOpToBits)
                 }

data Imm01 = Imm01 { imm01Bit :: Word8
                   } deriving (Eq, Ord, Show)

instance PP.Pretty Imm01 where
  pPrint _ = PP.text "Imm01: not implemented"

instance A.Arbitrary Imm01 where
  arbitrary g = Imm01 <$> A.arbitrary g

imm01BitField :: Field
imm01BitField = Field 1 0

imm01ToBits :: Imm01 -> Word32
imm01ToBits val =
  insert imm01BitField (imm01Bit val) 0

mkImm01 :: Word32 -> Imm01
mkImm01 w =
  Imm01 (fromIntegral $ extract imm01BitField w)

imm01Operand :: OperandPayload
imm01Operand =
  OperandPayload { opTypeT = [t| Imm01 |]
                 , opConE  = Just (varE 'mkImm01)
                 , opWordE = Just (varE 'imm01ToBits)
                 }

data Imm0127 = Imm0127 { imm0127Imm :: Word8
                       } deriving (Eq, Ord, Show)

instance PP.Pretty Imm0127 where
  pPrint _ = PP.text "Imm0127: not implemented"

instance A.Arbitrary Imm0127 where
  arbitrary g = Imm0127 <$> A.arbitrary g

imm0127ImmField :: Field
imm0127ImmField = Field 7 0

imm0127ToBits :: Imm0127 -> Word32
imm0127ToBits val =
  insert imm0127ImmField (imm0127Imm val) 0

mkImm0127 :: Word32 -> Imm0127
mkImm0127 w =
  Imm0127 (fromIntegral $ extract imm0127ImmField w)

imm0127Operand :: OperandPayload
imm0127Operand =
  OperandPayload { opTypeT = [t| Imm0127 |]
                 , opConE  = Just (varE 'mkImm0127)
                 , opWordE = Just (varE 'imm0127ToBits)
                 }

data Imm015 = Imm015 { imm015Imm :: Word8
                     } deriving (Eq, Ord, Show)

instance PP.Pretty Imm015 where
  pPrint _ = PP.text "Imm015: not implemented"

instance A.Arbitrary Imm015 where
  arbitrary g = Imm015 <$> A.arbitrary g

imm015ImmField :: Field
imm015ImmField = Field 4 0

imm015ToBits :: Imm015 -> Word32
imm015ToBits val =
  insert imm015ImmField (imm015Imm val) 0

mkImm015 :: Word32 -> Imm015
mkImm015 w =
  Imm015 (fromIntegral $ extract imm015ImmField w)

imm015Operand :: OperandPayload
imm015Operand =
  OperandPayload { opTypeT = [t| Imm015 |]
                 , opConE  = Just (varE 'mkImm015)
                 , opWordE = Just (varE 'imm015ToBits)
                 }

data Imm031 = Imm031 { imm031Imm :: Word8
                     } deriving (Eq, Ord, Show)

instance PP.Pretty Imm031 where
  pPrint _ = PP.text "Imm031: not implemented"

instance A.Arbitrary Imm031 where
  arbitrary g = Imm031 <$> A.arbitrary g

imm031ImmField :: Field
imm031ImmField = Field 5 0

imm031ToBits :: Imm031 -> Word32
imm031ToBits val =
  insert imm031ImmField (imm031Imm val) 0

mkImm031 :: Word32 -> Imm031
mkImm031 w =
  Imm031 (fromIntegral $ extract imm031ImmField w)

imm031Operand :: OperandPayload
imm031Operand =
  OperandPayload { opTypeT = [t| Imm031 |]
                 , opConE  = Just (varE 'mkImm031)
                 , opWordE = Just (varE 'imm031ToBits)
                 }

data Imm063 = Imm063 { imm063Imm :: Word8
                     } deriving (Eq, Ord, Show)

instance PP.Pretty Imm063 where
  pPrint _ = PP.text "Imm063: not implemented"

instance A.Arbitrary Imm063 where
  arbitrary g = Imm063 <$> A.arbitrary g

imm063ImmField :: Field
imm063ImmField = Field 6 0

imm063ToBits :: Imm063 -> Word32
imm063ToBits val =
  insert imm063ImmField (imm063Imm val) 0

mkImm063 :: Word32 -> Imm063
mkImm063 w =
  Imm063 (fromIntegral $ extract imm063ImmField w)

imm063Operand :: OperandPayload
imm063Operand =
  OperandPayload { opTypeT = [t| Imm063 |]
                 , opConE  = Just (varE 'mkImm063)
                 , opWordE = Just (varE 'imm063ToBits)
                 }

data Imm065535 = Imm065535 { imm065535Imm :: Word16
                           } deriving (Eq, Ord, Show)

instance PP.Pretty Imm065535 where
  pPrint _ = PP.text "Imm065535: not implemented"

instance A.Arbitrary Imm065535 where
  arbitrary g = Imm065535 <$> A.arbitrary g

imm065535ImmField :: Field
imm065535ImmField = Field 16 0

imm065535ToBits :: Imm065535 -> Word32
imm065535ToBits val =
  insert imm065535ImmField (imm065535Imm val) 0

mkImm065535 :: Word32 -> Imm065535
mkImm065535 w =
  Imm065535 (fromIntegral $ extract imm065535ImmField w)

imm065535Operand :: OperandPayload
imm065535Operand =
  OperandPayload { opTypeT = [t| Imm065535 |]
                 , opConE  = Just (varE 'mkImm065535)
                 , opWordE = Just (varE 'imm065535ToBits)
                 }

data Imm07 = Imm07 { imm07Imm :: Word8
                   } deriving (Eq, Ord, Show)

instance PP.Pretty Imm07 where
  pPrint _ = PP.text "Imm07: not implemented"

instance A.Arbitrary Imm07 where
  arbitrary g = Imm07 <$> A.arbitrary g

imm07ImmField :: Field
imm07ImmField = Field 3 0

imm07ToBits :: Imm07 -> Word32
imm07ToBits val =
  insert imm07ImmField (imm07Imm val) 0

mkImm07 :: Word32 -> Imm07
mkImm07 w =
  Imm07 (fromIntegral $ extract imm07ImmField w)

imm07Operand :: OperandPayload
imm07Operand =
  OperandPayload { opTypeT = [t| Imm07 |]
                 , opConE  = Just (varE 'mkImm07)
                 , opWordE = Just (varE 'imm07ToBits)
                 }

data Imm32015 = Imm32015 { imm32015Nzcv :: Word8
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Imm32015 where
  pPrint _ = PP.text "Imm32015: not implemented"

instance A.Arbitrary Imm32015 where
  arbitrary g = Imm32015 <$> A.arbitrary g

imm32015NzcvField :: Field
imm32015NzcvField = Field 4 0

imm32015ToBits :: Imm32015 -> Word32
imm32015ToBits val =
  insert imm32015NzcvField (imm32015Nzcv val) 0

mkImm32015 :: Word32 -> Imm32015
mkImm32015 w =
  Imm32015 (fromIntegral $ extract imm32015NzcvField w)

imm32015Operand :: OperandPayload
imm32015Operand =
  OperandPayload { opTypeT = [t| Imm32015 |]
                 , opConE  = Just (varE 'mkImm32015)
                 , opWordE = Just (varE 'imm32015ToBits)
                 }

data Imm32031 = Imm32031 { imm32031Imm :: Word8
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Imm32031 where
  pPrint _ = PP.text "Imm32031: not implemented"

instance A.Arbitrary Imm32031 where
  arbitrary g = Imm32031 <$> A.arbitrary g

imm32031ImmField :: Field
imm32031ImmField = Field 5 0

imm32031ToBits :: Imm32031 -> Word32
imm32031ToBits val =
  insert imm32031ImmField (imm32031Imm val) 0

mkImm32031 :: Word32 -> Imm32031
mkImm32031 w =
  Imm32031 (fromIntegral $ extract imm32031ImmField w)

imm32031Operand :: OperandPayload
imm32031Operand =
  OperandPayload { opTypeT = [t| Imm32031 |]
                 , opConE  = Just (varE 'mkImm32031)
                 , opWordE = Just (varE 'imm32031ToBits)
                 }

data LogicalImm32 = LogicalImm32 { logicalImm32Imms :: Word8
                                 , logicalImm32Immr :: Word8
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty LogicalImm32 where
  pPrint _ = PP.text "LogicalImm32: not implemented"

instance A.Arbitrary LogicalImm32 where
  arbitrary g = LogicalImm32 <$> A.arbitrary g <*> A.arbitrary g

logicalImm32ImmsField :: Field
logicalImm32ImmsField = Field 6 0

logicalImm32ImmrField :: Field
logicalImm32ImmrField = Field 6 6

logicalImm32ToBits :: LogicalImm32 -> Word32
logicalImm32ToBits val =
  insert logicalImm32ImmsField (logicalImm32Imms val) $
  insert logicalImm32ImmrField (logicalImm32Immr val) 0

mkLogicalImm32 :: Word32 -> LogicalImm32
mkLogicalImm32 w =
  LogicalImm32 (fromIntegral $ extract logicalImm32ImmsField w)
               (fromIntegral $ extract logicalImm32ImmrField w)

logicalImm32Operand :: OperandPayload
logicalImm32Operand =
  OperandPayload { opTypeT = [t| LogicalImm32 |]
                 , opConE  = Just (varE 'mkLogicalImm32)
                 , opWordE = Just (varE 'logicalImm32ToBits)
                 }

data LogicalImm64 = LogicalImm64 { logicalImm64Imms :: Word8
                                 , logicalImm64Immr :: Word8
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty LogicalImm64 where
  pPrint _ = PP.text "LogicalImm64: not implemented"

instance A.Arbitrary LogicalImm64 where
  arbitrary g = LogicalImm64 <$> A.arbitrary g <*> A.arbitrary g

logicalImm64ImmsField :: Field
logicalImm64ImmsField = Field 6 0

logicalImm64ImmrField :: Field
logicalImm64ImmrField = Field 6 6

logicalImm64ToBits :: LogicalImm64 -> Word32
logicalImm64ToBits val =
  insert logicalImm64ImmsField (logicalImm64Imms val) $
  insert logicalImm64ImmrField (logicalImm64Immr val) 0

mkLogicalImm64 :: Word32 -> LogicalImm64
mkLogicalImm64 w =
  LogicalImm64 (fromIntegral $ extract logicalImm64ImmsField w)
               (fromIntegral $ extract logicalImm64ImmrField w)

logicalImm64Operand :: OperandPayload
logicalImm64Operand =
  OperandPayload { opTypeT = [t| LogicalImm64 |]
                 , opConE  = Just (varE 'mkLogicalImm64)
                 , opWordE = Just (varE 'logicalImm64ToBits)
                 }

data Movimm32Imm = Movimm32Imm { movimm32ImmImm :: Word16
                               } deriving (Eq, Ord, Show)

instance PP.Pretty Movimm32Imm where
  pPrint _ = PP.text "Movimm32Imm: not implemented"

instance A.Arbitrary Movimm32Imm where
  arbitrary g = Movimm32Imm <$> A.arbitrary g

movimm32ImmImmField :: Field
movimm32ImmImmField = Field 16 0

movimm32ImmToBits :: Movimm32Imm -> Word32
movimm32ImmToBits val =
  insert movimm32ImmImmField (movimm32ImmImm val) 0

mkMovimm32Imm :: Word32 -> Movimm32Imm
mkMovimm32Imm w =
  Movimm32Imm (fromIntegral $ extract movimm32ImmImmField w)

movimm32ImmOperand :: OperandPayload
movimm32ImmOperand =
  OperandPayload { opTypeT = [t| Movimm32Imm |]
                 , opConE  = Just (varE 'mkMovimm32Imm)
                 , opWordE = Just (varE 'movimm32ImmToBits)
                 }

data Movimm32Shift = Movimm32Shift { movimm32ShiftShift :: Word8
                                   } deriving (Eq, Ord, Show)

instance PP.Pretty Movimm32Shift where
  pPrint _ = PP.text "Movimm32Shift: not implemented"

instance A.Arbitrary Movimm32Shift where
  arbitrary g = Movimm32Shift <$> A.arbitrary g

movimm32ShiftShiftField :: Field
movimm32ShiftShiftField = Field 2 4

movimm32ShiftToBits :: Movimm32Shift -> Word32
movimm32ShiftToBits val =
  insert movimm32ShiftShiftField (movimm32ShiftShift val) 0

mkMovimm32Shift :: Word32 -> Movimm32Shift
mkMovimm32Shift w =
  Movimm32Shift (fromIntegral $ extract movimm32ShiftShiftField w)

movimm32ShiftOperand :: OperandPayload
movimm32ShiftOperand =
  OperandPayload { opTypeT = [t| Movimm32Shift |]
                 , opConE  = Just (varE 'mkMovimm32Shift)
                 , opWordE = Just (varE 'movimm32ShiftToBits)
                 }

data Movimm64Shift = Movimm64Shift { movimm64ShiftShift :: Word8
                                   } deriving (Eq, Ord, Show)

instance PP.Pretty Movimm64Shift where
  pPrint _ = PP.text "Movimm64Shift: not implemented"

instance A.Arbitrary Movimm64Shift where
  arbitrary g = Movimm64Shift <$> A.arbitrary g

movimm64ShiftShiftField :: Field
movimm64ShiftShiftField = Field 2 4

movimm64ShiftToBits :: Movimm64Shift -> Word32
movimm64ShiftToBits val =
  insert movimm64ShiftShiftField (movimm64ShiftShift val) 0

mkMovimm64Shift :: Word32 -> Movimm64Shift
mkMovimm64Shift w =
  Movimm64Shift (fromIntegral $ extract movimm64ShiftShiftField w)

movimm64ShiftOperand :: OperandPayload
movimm64ShiftOperand =
  OperandPayload { opTypeT = [t| Movimm64Shift |]
                 , opConE  = Just (varE 'mkMovimm64Shift)
                 , opWordE = Just (varE 'movimm64ShiftToBits)
                 }

data MrsSysregOp = MrsSysregOp { mrsSysregOpOp2 :: Word8
                               , mrsSysregOpCrm :: Word8
                               , mrsSysregOpCrn :: Word8
                               , mrsSysregOpOp1 :: Word8
                               , mrsSysregOpO0 :: Word8
                               , mrsSysregOpHibit :: Word8
                               } deriving (Eq, Ord, Show)

instance PP.Pretty MrsSysregOp where
  pPrint _ = PP.text "MrsSysregOp: not implemented"

instance A.Arbitrary MrsSysregOp where
  arbitrary g = MrsSysregOp <$> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g

mrsSysregOpOp2Field :: Field
mrsSysregOpOp2Field = Field 3 0

mrsSysregOpCrmField :: Field
mrsSysregOpCrmField = Field 4 3

mrsSysregOpCrnField :: Field
mrsSysregOpCrnField = Field 4 7

mrsSysregOpOp1Field :: Field
mrsSysregOpOp1Field = Field 3 11

mrsSysregOpO0Field :: Field
mrsSysregOpO0Field = Field 1 14

mrsSysregOpHibitField :: Field
mrsSysregOpHibitField = Field 1 15

mrsSysregOpToBits :: MrsSysregOp -> Word32
mrsSysregOpToBits val =
  insert mrsSysregOpOp2Field (mrsSysregOpOp2 val) $
  insert mrsSysregOpCrmField (mrsSysregOpCrm val) $
  insert mrsSysregOpCrnField (mrsSysregOpCrn val) $
  insert mrsSysregOpOp1Field (mrsSysregOpOp1 val) $
  insert mrsSysregOpO0Field (mrsSysregOpO0 val) $
  insert mrsSysregOpHibitField (mrsSysregOpHibit val) 0

mkMrsSysregOp :: Word32 -> MrsSysregOp
mkMrsSysregOp w =
  MrsSysregOp (fromIntegral $ extract mrsSysregOpOp2Field w)
              (fromIntegral $ extract mrsSysregOpCrmField w)
              (fromIntegral $ extract mrsSysregOpCrnField w)
              (fromIntegral $ extract mrsSysregOpOp1Field w)
              (fromIntegral $ extract mrsSysregOpO0Field w)
              (fromIntegral $ extract mrsSysregOpHibitField w)

mrsSysregOpOperand :: OperandPayload
mrsSysregOpOperand =
  OperandPayload { opTypeT = [t| MrsSysregOp |]
                 , opConE  = Just (varE 'mkMrsSysregOp)
                 , opWordE = Just (varE 'mrsSysregOpToBits)
                 }

data MsrSysregOp = MsrSysregOp { msrSysregOpOp2 :: Word8
                               , msrSysregOpCrm :: Word8
                               , msrSysregOpCrn :: Word8
                               , msrSysregOpOp1 :: Word8
                               , msrSysregOpO0 :: Word8
                               , msrSysregOpHibit :: Word8
                               } deriving (Eq, Ord, Show)

instance PP.Pretty MsrSysregOp where
  pPrint _ = PP.text "MsrSysregOp: not implemented"

instance A.Arbitrary MsrSysregOp where
  arbitrary g = MsrSysregOp <$> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g

msrSysregOpOp2Field :: Field
msrSysregOpOp2Field = Field 3 0

msrSysregOpCrmField :: Field
msrSysregOpCrmField = Field 4 3

msrSysregOpCrnField :: Field
msrSysregOpCrnField = Field 4 7

msrSysregOpOp1Field :: Field
msrSysregOpOp1Field = Field 3 11

msrSysregOpO0Field :: Field
msrSysregOpO0Field = Field 1 14

msrSysregOpHibitField :: Field
msrSysregOpHibitField = Field 1 15

msrSysregOpToBits :: MsrSysregOp -> Word32
msrSysregOpToBits val =
  insert msrSysregOpOp2Field (msrSysregOpOp2 val) $
  insert msrSysregOpCrmField (msrSysregOpCrm val) $
  insert msrSysregOpCrnField (msrSysregOpCrn val) $
  insert msrSysregOpOp1Field (msrSysregOpOp1 val) $
  insert msrSysregOpO0Field (msrSysregOpO0 val) $
  insert msrSysregOpHibitField (msrSysregOpHibit val) 0

mkMsrSysregOp :: Word32 -> MsrSysregOp
mkMsrSysregOp w =
  MsrSysregOp (fromIntegral $ extract msrSysregOpOp2Field w)
              (fromIntegral $ extract msrSysregOpCrmField w)
              (fromIntegral $ extract msrSysregOpCrnField w)
              (fromIntegral $ extract msrSysregOpOp1Field w)
              (fromIntegral $ extract msrSysregOpO0Field w)
              (fromIntegral $ extract msrSysregOpHibitField w)

msrSysregOpOperand :: OperandPayload
msrSysregOpOperand =
  OperandPayload { opTypeT = [t| MsrSysregOp |]
                 , opConE  = Just (varE 'mkMsrSysregOp)
                 , opWordE = Just (varE 'msrSysregOpToBits)
                 }

data Prfop = Prfop { prfopType :: Word8
                   } deriving (Eq, Ord, Show)

instance PP.Pretty Prfop where
  -- See C5.6.144 for prfop values
  pPrint _ = PP.text "Prfop: not implemented"

instance A.Arbitrary Prfop where
  arbitrary g = Prfop <$> A.arbitrary g

prfopTypeField :: Field
prfopTypeField = Field 5 0

prfopToBits :: Prfop -> Word32
prfopToBits val =
  insert prfopTypeField (prfopType val) 0

mkPrfop :: Word32 -> Prfop
mkPrfop w =
  Prfop (fromIntegral $ extract prfopTypeField w)

prfopOperand :: OperandPayload
prfopOperand =
  OperandPayload { opTypeT = [t| Prfop |]
                 , opConE  = Just (varE 'mkPrfop)
                 , opWordE = Just (varE 'prfopToBits)
                 }

data Pstatefield1Op = Pstatefield1Op { pstatefield1OpOp1 :: Word8
                                     , pstatefield1OpOp2 :: Word8
                                     } deriving (Eq, Ord, Show)

instance PP.Pretty Pstatefield1Op where
  -- See C5.6.130
  pPrint _ = PP.text "Pstatefield1Op: not implemented"

instance A.Arbitrary Pstatefield1Op where
  arbitrary g = Pstatefield1Op <$> A.arbitrary g <*> A.arbitrary g

pstatefield1OpOp1Field :: Field
pstatefield1OpOp1Field = Field 3 3

pstatefield1OpOp2Field :: Field
pstatefield1OpOp2Field = Field 3 0

pstatefield1OpToBits :: Pstatefield1Op -> Word32
pstatefield1OpToBits val =
  insert pstatefield1OpOp1Field (pstatefield1OpOp1 val) $
  insert pstatefield1OpOp2Field (pstatefield1OpOp2 val) 0

mkPstatefield1Op :: Word32 -> Pstatefield1Op
mkPstatefield1Op w =
  Pstatefield1Op (fromIntegral $ extract pstatefield1OpOp1Field w)
                 (fromIntegral $ extract pstatefield1OpOp2Field w)

pstatefield1OpOperand :: OperandPayload
pstatefield1OpOperand =
  OperandPayload { opTypeT = [t| Pstatefield1Op |]
                 , opConE  = Just (varE 'mkPstatefield1Op)
                 , opWordE = Just (varE 'pstatefield1OpToBits)
                 }

data Pstatefield4Op = Pstatefield4Op { pstatefield4OpOp1 :: Word8
                                     , pstatefield4OpOp2 :: Word8
                                     } deriving (Eq, Ord, Show)

instance PP.Pretty Pstatefield4Op where
  pPrint _ = PP.text "Pstatefield4Op: not implemented"

instance A.Arbitrary Pstatefield4Op where
  arbitrary g = Pstatefield4Op <$> A.arbitrary g <*> A.arbitrary g

pstatefield4OpOp1Field :: Field
pstatefield4OpOp1Field = Field 3 3

pstatefield4OpOp2Field :: Field
pstatefield4OpOp2Field = Field 3 0

pstatefield4OpToBits :: Pstatefield4Op -> Word32
pstatefield4OpToBits val =
  insert pstatefield4OpOp1Field (pstatefield4OpOp1 val) $
  insert pstatefield4OpOp2Field (pstatefield4OpOp2 val) 0

mkPstatefield4Op :: Word32 -> Pstatefield4Op
mkPstatefield4Op w =
  Pstatefield4Op (fromIntegral $ extract pstatefield4OpOp1Field w)
                 (fromIntegral $ extract pstatefield4OpOp2Field w)

pstatefield4OpOperand :: OperandPayload
pstatefield4OpOperand =
  OperandPayload { opTypeT = [t| Pstatefield4Op |]
                 , opConE  = Just (varE 'mkPstatefield4Op)
                 , opWordE = Just (varE 'pstatefield4OpToBits)
                 }

data RoWextend128 = RoWextend128 { roWextend128Sbit :: Word8
                                 , roWextend128OptionHiBit :: Word8
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty RoWextend128 where
  -- See C6.3.168
  pPrint _ = PP.text "RoWextend128: not implemented"

instance A.Arbitrary RoWextend128 where
  arbitrary g = RoWextend128 <$> A.arbitrary g <*> A.arbitrary g

roWextend128SbitField :: Field
roWextend128SbitField = Field 1 0

roWextend128OptionHiBitField :: Field
roWextend128OptionHiBitField = Field 1 1

roWextend128ToBits :: RoWextend128 -> Word32
roWextend128ToBits val =
  insert roWextend128SbitField (roWextend128Sbit val) $
  insert roWextend128OptionHiBitField (roWextend128OptionHiBit val) 0

mkRoWextend128 :: Word32 -> RoWextend128
mkRoWextend128 w =
  RoWextend128 (fromIntegral $ extract roWextend128SbitField w)
               (fromIntegral $ extract roWextend128OptionHiBitField w)

roWextend128Operand :: OperandPayload
roWextend128Operand =
  OperandPayload { opTypeT = [t| RoWextend128 |]
                 , opConE  = Just (varE 'mkRoWextend128)
                 , opWordE = Just (varE 'roWextend128ToBits)
                 }

