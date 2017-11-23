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

  , RoWextend16
  , mkRoWextend16
  , roWextend16ToBits
  , roWextend16Operand

  , RoWextend32
  , mkRoWextend32
  , roWextend32ToBits
  , roWextend32Operand

  , RoWextend64
  , mkRoWextend64
  , roWextend64ToBits
  , roWextend64Operand

  , RoWextend8
  , mkRoWextend8
  , roWextend8ToBits
  , roWextend8Operand

  , RoXextend128
  , mkRoXextend128
  , roXextend128ToBits
  , roXextend128Operand

  , RoXextend16
  , mkRoXextend16
  , roXextend16ToBits
  , roXextend16Operand

  , RoXextend32
  , mkRoXextend32
  , roXextend32ToBits
  , roXextend32Operand

  , RoXextend64
  , mkRoXextend64
  , roXextend64ToBits
  , roXextend64Operand

  , RoXextend8
  , mkRoXextend8
  , roXextend8ToBits
  , roXextend8Operand

  , Simm7s16
  , mkSimm7s16
  , simm7s16ToBits
  , simm7s16Operand

  , Simm7s4
  , mkSimm7s4
  , simm7s4ToBits
  , simm7s4Operand

  , Simm7s8
  , mkSimm7s8
  , simm7s8ToBits
  , simm7s8Operand

  , Simm9
  , mkSimm9
  , simm9ToBits
  , simm9Operand

  , SysCrOp
  , mkSysCrOp
  , sysCrOpToBits
  , sysCrOpOperand

  , TbzImm031Diag
  , mkTbzImm031Diag
  , tbzImm031DiagToBits
  , tbzImm031DiagOperand

  , TbzImm3263
  , mkTbzImm3263
  , tbzImm3263ToBits
  , tbzImm3263Operand

  , Uimm12s1
  , mkUimm12s1
  , uimm12s1ToBits
  , uimm12s1Operand

  , Uimm12s16
  , mkUimm12s16
  , uimm12s16ToBits
  , uimm12s16Operand

  , Uimm12s2
  , mkUimm12s2
  , uimm12s2ToBits
  , uimm12s2Operand

  , Uimm12s4
  , mkUimm12s4
  , uimm12s4ToBits
  , uimm12s4Operand

  , Uimm12s8
  , mkUimm12s8
  , uimm12s8ToBits
  , uimm12s8Operand

  , Addext
  , mkAddext
  , addextToBits
  , addextOperand

  , FixedpointF32I32
  , mkFixedpointF32I32
  , fixedpointF32I32ToBits
  , fixedpointF32I32Operand

  , FixedpointF16I32
  , mkFixedpointF16I32
  , fixedpointF16I32ToBits
  , fixedpointF16I32Operand

  , FixedpointF16I64
  , mkFixedpointF16I64
  , fixedpointF16I64ToBits
  , fixedpointF16I64Operand

  , FixedpointF32I64
  , mkFixedpointF32I64
  , fixedpointF32I64ToBits
  , fixedpointF32I64Operand

  , FixedpointF64I32
  , mkFixedpointF64I32
  , fixedpointF64I32ToBits
  , fixedpointF64I32Operand

  , FixedpointF64I64
  , mkFixedpointF64I64
  , fixedpointF64I64ToBits
  , fixedpointF64I64Operand

  , Fpimm16
  , mkFpimm16
  , fpimm16ToBits
  , fpimm16Operand

  , Fpimm32
  , mkFpimm32
  , fpimm32ToBits
  , fpimm32Operand

  , Fpimm64
  , mkFpimm64
  , fpimm64ToBits
  , fpimm64Operand

  )
where

import Data.Bits
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Int ( Int16, Int32, Int64, Int8 )

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
  pPrint (FPR128 r) = PP.text $ "q" <> show r

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
  pPrint (FPR16 r) = PP.text $ "h" <> show r

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
  pPrint (FPR32 r) = PP.text $ "s" <> show r

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
  pPrint (FPR64 r) = PP.text $ "d" <> show r

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
  pPrint (FPR8 r) = PP.text $ "b" <> show r

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
  pPrint (GPR32 31) = PP.text "wzr"
  pPrint (GPR32 r) = PP.text $ "w" <> show r

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
  pPrint (GPR64 r) = PP.text $ "x" <> show r

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
  pPrint (GPR32sp 0b11111) = PP.text "wsp"
  pPrint (GPR32sp r) = PP.text $ "w" <> show r

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
  pPrint (GPR64sp 0b11111) = PP.text "sp"
  pPrint (GPR64sp r) = PP.text $ "x" <> show r

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
  pPrint (V128 r) = PP.text $ "v" <> show r

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
  pPrint (AddsubShiftedImm32 imm shift) =
      let i :: Word32
          i = case shift of
                0b0 -> fromIntegral imm
                0b1 -> (fromIntegral imm) `shiftL` 12
                _ -> error $ "Invalid AddsubShiftedImm32 value: " <> show shift
      in PP.text "#0x" <> (PP.text $ showHex i "")

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
  pPrint (AddsubShiftedImm64 imm shift) =
      let shiftStr = case shift of
                0b0 -> mempty -- Default lsl #0, omit
                0b1 -> PP.text ", lsl #12"
                _ -> error $ "invalid AddsubShiftedImm64 value: " <> show shift
      in (PP.text "#0x" <> PP.text (showHex imm "")) <> shiftStr

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

data RoWextend16 = RoWextend16 { roWextend16Sbit :: Word8
                               , roWextend16OptionHiBit :: Word8
                               } deriving (Eq, Ord, Show)

instance PP.Pretty RoWextend16 where
  -- See C5.6.93
  pPrint _ = PP.text "RoWextend16: not implemented"

instance A.Arbitrary RoWextend16 where
  arbitrary g = RoWextend16 <$> A.arbitrary g <*> A.arbitrary g

roWextend16SbitField :: Field
roWextend16SbitField = Field 1 0

roWextend16OptionHiBitField :: Field
roWextend16OptionHiBitField = Field 1 1

roWextend16ToBits :: RoWextend16 -> Word32
roWextend16ToBits val =
  insert roWextend16SbitField (roWextend16Sbit val) $
  insert roWextend16OptionHiBitField (roWextend16OptionHiBit val) 0

mkRoWextend16 :: Word32 -> RoWextend16
mkRoWextend16 w =
  RoWextend16 (fromIntegral $ extract roWextend16SbitField w)
              (fromIntegral $ extract roWextend16OptionHiBitField w)

roWextend16Operand :: OperandPayload
roWextend16Operand =
  OperandPayload { opTypeT = [t| RoWextend16 |]
                 , opConE  = Just (varE 'mkRoWextend16)
                 , opWordE = Just (varE 'roWextend16ToBits)
                 }

data RoWextend32 = RoWextend32 { roWextend32Sbit :: Word8
                               , roWextend32OptionHiBit :: Word8
                               } deriving (Eq, Ord, Show)

instance PP.Pretty RoWextend32 where
  pPrint (RoWextend32 s o) =
      let ty = case o of
            0b0 -> "uxtw"
            0b1 -> "sxtw"
            _   -> error $ "Invalid RoWextend32 value: " <> show o
          amt = if s == 1 then 2 else 0
      in PP.text ty PP.<+> PP.text (show amt)

instance A.Arbitrary RoWextend32 where
  arbitrary g = RoWextend32 <$> A.arbitrary g <*> A.arbitrary g

roWextend32SbitField :: Field
roWextend32SbitField = Field 1 0

roWextend32OptionHiBitField :: Field
roWextend32OptionHiBitField = Field 1 1

roWextend32ToBits :: RoWextend32 -> Word32
roWextend32ToBits val =
  insert roWextend32SbitField (roWextend32Sbit val) $
  insert roWextend32OptionHiBitField (roWextend32OptionHiBit val) 0

mkRoWextend32 :: Word32 -> RoWextend32
mkRoWextend32 w =
  RoWextend32 (fromIntegral $ extract roWextend32SbitField w)
              (fromIntegral $ extract roWextend32OptionHiBitField w)

roWextend32Operand :: OperandPayload
roWextend32Operand =
  OperandPayload { opTypeT = [t| RoWextend32 |]
                 , opConE  = Just (varE 'mkRoWextend32)
                 , opWordE = Just (varE 'roWextend32ToBits)
                 }

data RoWextend64 = RoWextend64 { roWextend64Sbit :: Word8
                               , roWextend64OptionHiBit :: Word8
                               } deriving (Eq, Ord, Show)

instance PP.Pretty RoWextend64 where
  pPrint _ = PP.text "RoWextend64: not implemented"

instance A.Arbitrary RoWextend64 where
  arbitrary g = RoWextend64 <$> A.arbitrary g <*> A.arbitrary g

roWextend64SbitField :: Field
roWextend64SbitField = Field 1 0

roWextend64OptionHiBitField :: Field
roWextend64OptionHiBitField = Field 1 1

roWextend64ToBits :: RoWextend64 -> Word32
roWextend64ToBits val =
  insert roWextend64SbitField (roWextend64Sbit val) $
  insert roWextend64OptionHiBitField (roWextend64OptionHiBit val) 0

mkRoWextend64 :: Word32 -> RoWextend64
mkRoWextend64 w =
  RoWextend64 (fromIntegral $ extract roWextend64SbitField w)
              (fromIntegral $ extract roWextend64OptionHiBitField w)

roWextend64Operand :: OperandPayload
roWextend64Operand =
  OperandPayload { opTypeT = [t| RoWextend64 |]
                 , opConE  = Just (varE 'mkRoWextend64)
                 , opWordE = Just (varE 'roWextend64ToBits)
                 }

data RoWextend8 = RoWextend8 { roWextend8Sbit :: Word8
                             , roWextend8OptionHiBit :: Word8
                             } deriving (Eq, Ord, Show)

instance PP.Pretty RoWextend8 where
  pPrint _ = PP.text "RoWextend8: not implemented"

instance A.Arbitrary RoWextend8 where
  arbitrary g = RoWextend8 <$> A.arbitrary g <*> A.arbitrary g

roWextend8SbitField :: Field
roWextend8SbitField = Field 1 0

roWextend8OptionHiBitField :: Field
roWextend8OptionHiBitField = Field 1 1

roWextend8ToBits :: RoWextend8 -> Word32
roWextend8ToBits val =
  insert roWextend8SbitField (roWextend8Sbit val) $
  insert roWextend8OptionHiBitField (roWextend8OptionHiBit val) 0

mkRoWextend8 :: Word32 -> RoWextend8
mkRoWextend8 w =
  RoWextend8 (fromIntegral $ extract roWextend8SbitField w)
             (fromIntegral $ extract roWextend8OptionHiBitField w)

roWextend8Operand :: OperandPayload
roWextend8Operand =
  OperandPayload { opTypeT = [t| RoWextend8 |]
                 , opConE  = Just (varE 'mkRoWextend8)
                 , opWordE = Just (varE 'roWextend8ToBits)
                 }

data RoXextend128 = RoXextend128 { roXextend128Sbit :: Word8
                                 , roXextend128OptionHiBit :: Word8
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty RoXextend128 where
  -- See C6.3.168
  pPrint _ = PP.text "RoXextend128: not implemented"

instance A.Arbitrary RoXextend128 where
  arbitrary g = RoXextend128 <$> A.arbitrary g <*> A.arbitrary g

roXextend128SbitField :: Field
roXextend128SbitField = Field 1 0

roXextend128OptionHiBitField :: Field
roXextend128OptionHiBitField = Field 1 1

roXextend128ToBits :: RoXextend128 -> Word32
roXextend128ToBits val =
  insert roXextend128SbitField (roXextend128Sbit val) $
  insert roXextend128OptionHiBitField (roXextend128OptionHiBit val) 0

mkRoXextend128 :: Word32 -> RoXextend128
mkRoXextend128 w =
  RoXextend128 (fromIntegral $ extract roXextend128SbitField w)
               (fromIntegral $ extract roXextend128OptionHiBitField w)

roXextend128Operand :: OperandPayload
roXextend128Operand =
  OperandPayload { opTypeT = [t| RoXextend128 |]
                 , opConE  = Just (varE 'mkRoXextend128)
                 , opWordE = Just (varE 'roXextend128ToBits)
                 }

data RoXextend16 = RoXextend16 { roXextend16Sbit :: Word8
                               , roXextend16OptionHiBit :: Word8
                               } deriving (Eq, Ord, Show)

instance PP.Pretty RoXextend16 where
  -- See C5.6.93
  pPrint _ = PP.text "RoXextend16: not implemented"

instance A.Arbitrary RoXextend16 where
  arbitrary g = RoXextend16 <$> A.arbitrary g <*> A.arbitrary g

roXextend16SbitField :: Field
roXextend16SbitField = Field 1 0

roXextend16OptionHiBitField :: Field
roXextend16OptionHiBitField = Field 1 1

roXextend16ToBits :: RoXextend16 -> Word32
roXextend16ToBits val =
  insert roXextend16SbitField (roXextend16Sbit val) $
  insert roXextend16OptionHiBitField (roXextend16OptionHiBit val) 0

mkRoXextend16 :: Word32 -> RoXextend16
mkRoXextend16 w =
  RoXextend16 (fromIntegral $ extract roXextend16SbitField w)
              (fromIntegral $ extract roXextend16OptionHiBitField w)

roXextend16Operand :: OperandPayload
roXextend16Operand =
  OperandPayload { opTypeT = [t| RoXextend16 |]
                 , opConE  = Just (varE 'mkRoXextend16)
                 , opWordE = Just (varE 'roXextend16ToBits)
                 }

data RoXextend32 = RoXextend32 { roXextend32Sbit :: Word8
                               , roXextend32OptionHiBit :: Word8
                               } deriving (Eq, Ord, Show)

instance PP.Pretty RoXextend32 where
  pPrint (RoXextend32 s o) =
      let ty = case o of
            0b0 -> "lsl"
            0b1 -> "sxtx"
            _   -> error $ "Invalid RoXextend32 value: " <> show o
          amt = if s == 1 then 2 else 0
      in PP.text ty PP.<+> PP.text (show amt)

instance A.Arbitrary RoXextend32 where
  arbitrary g = RoXextend32 <$> A.arbitrary g <*> A.arbitrary g

roXextend32SbitField :: Field
roXextend32SbitField = Field 1 0

roXextend32OptionHiBitField :: Field
roXextend32OptionHiBitField = Field 1 1

roXextend32ToBits :: RoXextend32 -> Word32
roXextend32ToBits val =
  insert roXextend32SbitField (roXextend32Sbit val) $
  insert roXextend32OptionHiBitField (roXextend32OptionHiBit val) 0

mkRoXextend32 :: Word32 -> RoXextend32
mkRoXextend32 w =
  RoXextend32 (fromIntegral $ extract roXextend32SbitField w)
              (fromIntegral $ extract roXextend32OptionHiBitField w)

roXextend32Operand :: OperandPayload
roXextend32Operand =
  OperandPayload { opTypeT = [t| RoXextend32 |]
                 , opConE  = Just (varE 'mkRoXextend32)
                 , opWordE = Just (varE 'roXextend32ToBits)
                 }

data RoXextend64 = RoXextend64 { roXextend64Sbit :: Word8
                               , roXextend64OptionHiBit :: Word8
                               } deriving (Eq, Ord, Show)

instance PP.Pretty RoXextend64 where
  pPrint (RoXextend64 s o) =
      let ty = case o of
            0b0 -> "lsl"
            0b1 -> "sxtx"
            _   -> error $ "Invalid RoXextend64 value: " <> show o
          amt = if s == 1 then 3 else 0
      in PP.text ty PP.<+> PP.text (show amt)

instance A.Arbitrary RoXextend64 where
  arbitrary g = RoXextend64 <$> A.arbitrary g <*> A.arbitrary g

roXextend64SbitField :: Field
roXextend64SbitField = Field 1 0

roXextend64OptionHiBitField :: Field
roXextend64OptionHiBitField = Field 1 1

roXextend64ToBits :: RoXextend64 -> Word32
roXextend64ToBits val =
  insert roXextend64SbitField (roXextend64Sbit val) $
  insert roXextend64OptionHiBitField (roXextend64OptionHiBit val) 0

mkRoXextend64 :: Word32 -> RoXextend64
mkRoXextend64 w =
  RoXextend64 (fromIntegral $ extract roXextend64SbitField w)
              (fromIntegral $ extract roXextend64OptionHiBitField w)

roXextend64Operand :: OperandPayload
roXextend64Operand =
  OperandPayload { opTypeT = [t| RoXextend64 |]
                 , opConE  = Just (varE 'mkRoXextend64)
                 , opWordE = Just (varE 'roXextend64ToBits)
                 }

data RoXextend8 = RoXextend8 { roXextend8Sbit :: Word8
                             , roXextend8OptionHiBit :: Word8
                             } deriving (Eq, Ord, Show)

instance PP.Pretty RoXextend8 where
  pPrint _ = PP.text "RoXextend8: not implemented"

instance A.Arbitrary RoXextend8 where
  arbitrary g = RoXextend8 <$> A.arbitrary g <*> A.arbitrary g

roXextend8SbitField :: Field
roXextend8SbitField = Field 1 0

roXextend8OptionHiBitField :: Field
roXextend8OptionHiBitField = Field 1 1

roXextend8ToBits :: RoXextend8 -> Word32
roXextend8ToBits val =
  insert roXextend8SbitField (roXextend8Sbit val) $
  insert roXextend8OptionHiBitField (roXextend8OptionHiBit val) 0

mkRoXextend8 :: Word32 -> RoXextend8
mkRoXextend8 w =
  RoXextend8 (fromIntegral $ extract roXextend8SbitField w)
             (fromIntegral $ extract roXextend8OptionHiBitField w)

roXextend8Operand :: OperandPayload
roXextend8Operand =
  OperandPayload { opTypeT = [t| RoXextend8 |]
                 , opConE  = Just (varE 'mkRoXextend8)
                 , opWordE = Just (varE 'roXextend8ToBits)
                 }

data Simm7s16 = Simm7s16 { simm7s16Imm :: Word8
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Simm7s16 where
  -- C5.6.80
  pPrint _ = PP.text "Simm7s16: not implemented"

instance A.Arbitrary Simm7s16 where
  arbitrary g = Simm7s16 <$> A.arbitrary g

simm7s16ImmField :: Field
simm7s16ImmField = Field 7 0

simm7s16ToBits :: Simm7s16 -> Word32
simm7s16ToBits val =
  insert simm7s16ImmField (simm7s16Imm val) 0

mkSimm7s16 :: Word32 -> Simm7s16
mkSimm7s16 w =
  Simm7s16 (fromIntegral $ extract simm7s16ImmField w)

simm7s16Operand :: OperandPayload
simm7s16Operand =
  OperandPayload { opTypeT = [t| Simm7s16 |]
                 , opConE  = Just (varE 'mkSimm7s16)
                 , opWordE = Just (varE 'simm7s16ToBits)
                 }

data Simm7s4 = Simm7s4 { simm7s4Imm :: Word8
                       } deriving (Eq, Ord, Show)

instance PP.Pretty Simm7s4 where
  pPrint _ = PP.text "Simm7s4: not implemented"

instance A.Arbitrary Simm7s4 where
  arbitrary g = Simm7s4 <$> A.arbitrary g

simm7s4ImmField :: Field
simm7s4ImmField = Field 7 0

simm7s4ToBits :: Simm7s4 -> Word32
simm7s4ToBits val =
  insert simm7s4ImmField (simm7s4Imm val) 0

mkSimm7s4 :: Word32 -> Simm7s4
mkSimm7s4 w =
  Simm7s4 (fromIntegral $ extract simm7s4ImmField w)

simm7s4Operand :: OperandPayload
simm7s4Operand =
  OperandPayload { opTypeT = [t| Simm7s4 |]
                 , opConE  = Just (varE 'mkSimm7s4)
                 , opWordE = Just (varE 'simm7s4ToBits)
                 }

data Simm7s8 = Simm7s8 { simm7s8Imm :: Word8
                       } deriving (Eq, Ord, Show)

instance PP.Pretty Simm7s8 where
  pPrint (Simm7s8 imm) =
      let signBit = 0b1000000
          -- Manual sign extension from a 7-bit value to 8-bit.
          v :: Int
          v = fromIntegral ((fromIntegral $ imm .|. ((imm .&. signBit) `shiftL` 1)) :: Int8)
      in PP.char '#' <> (PP.text $ show $ v `shiftL` 3)

instance A.Arbitrary Simm7s8 where
  arbitrary g = Simm7s8 <$> A.arbitrary g

simm7s8ImmField :: Field
simm7s8ImmField = Field 7 0

simm7s8ToBits :: Simm7s8 -> Word32
simm7s8ToBits val =
  insert simm7s8ImmField (simm7s8Imm val) 0

mkSimm7s8 :: Word32 -> Simm7s8
mkSimm7s8 w =
  Simm7s8 (fromIntegral $ extract simm7s8ImmField w)

simm7s8Operand :: OperandPayload
simm7s8Operand =
  OperandPayload { opTypeT = [t| Simm7s8 |]
                 , opConE  = Just (varE 'mkSimm7s8)
                 , opWordE = Just (varE 'simm7s8ToBits)
                 }

data Simm9 = Simm9 { simm9Imm :: Word16
                   } deriving (Eq, Ord, Show)

instance PP.Pretty Simm9 where
  -- C5.6.86
  pPrint _ = PP.text "Simm9: not implemented"

instance A.Arbitrary Simm9 where
  arbitrary g = Simm9 <$> A.arbitrary g

simm9ImmField :: Field
simm9ImmField = Field 9 0

simm9ToBits :: Simm9 -> Word32
simm9ToBits val =
  insert simm9ImmField (simm9Imm val) 0

mkSimm9 :: Word32 -> Simm9
mkSimm9 w =
  Simm9 (fromIntegral $ extract simm9ImmField w)

simm9Operand :: OperandPayload
simm9Operand =
  OperandPayload { opTypeT = [t| Simm9 |]
                 , opConE  = Just (varE 'mkSimm9)
                 , opWordE = Just (varE 'simm9ToBits)
                 }

data SysCrOp = SysCrOp { sysCrOpVal :: Word8
                       } deriving (Eq, Ord, Show)

instance PP.Pretty SysCrOp where
  pPrint _ = PP.text "SysCrOp: not implemented"

instance A.Arbitrary SysCrOp where
  arbitrary g = SysCrOp <$> A.arbitrary g

sysCrOpValField :: Field
sysCrOpValField = Field 4 0

sysCrOpToBits :: SysCrOp -> Word32
sysCrOpToBits val =
  insert sysCrOpValField (sysCrOpVal val) 0

mkSysCrOp :: Word32 -> SysCrOp
mkSysCrOp w =
  SysCrOp (fromIntegral $ extract sysCrOpValField w)

sysCrOpOperand :: OperandPayload
sysCrOpOperand =
  OperandPayload { opTypeT = [t| SysCrOp |]
                 , opConE  = Just (varE 'mkSysCrOp)
                 , opWordE = Just (varE 'sysCrOpToBits)
                 }

data TbzImm031Diag = TbzImm031Diag { tbzImm031DiagImm :: Word8
                                   } deriving (Eq, Ord, Show)

instance PP.Pretty TbzImm031Diag where
  -- See C5.6.206
  pPrint _ = PP.text "TbzImm031Diag: not implemented"

instance A.Arbitrary TbzImm031Diag where
  arbitrary g = TbzImm031Diag <$> A.arbitrary g

tbzImm031DiagImmField :: Field
tbzImm031DiagImmField = Field 5 0

tbzImm031DiagToBits :: TbzImm031Diag -> Word32
tbzImm031DiagToBits val =
  insert tbzImm031DiagImmField (tbzImm031DiagImm val) 0

mkTbzImm031Diag :: Word32 -> TbzImm031Diag
mkTbzImm031Diag w =
  TbzImm031Diag (fromIntegral $ extract tbzImm031DiagImmField w)

tbzImm031DiagOperand :: OperandPayload
tbzImm031DiagOperand =
  OperandPayload { opTypeT = [t| TbzImm031Diag |]
                 , opConE  = Just (varE 'mkTbzImm031Diag)
                 , opWordE = Just (varE 'tbzImm031DiagToBits)
                 }

data TbzImm3263 = TbzImm3263 { tbzImm3263Imm :: Word8
                             } deriving (Eq, Ord, Show)

instance PP.Pretty TbzImm3263 where
  -- See C5.6.206
  pPrint _ = PP.text "TbzImm3263: not implemented"

instance A.Arbitrary TbzImm3263 where
  arbitrary g = TbzImm3263 <$> A.arbitrary g

tbzImm3263ImmField :: Field
tbzImm3263ImmField = Field 5 0

tbzImm3263ToBits :: TbzImm3263 -> Word32
tbzImm3263ToBits val =
  insert tbzImm3263ImmField (tbzImm3263Imm val) 0

mkTbzImm3263 :: Word32 -> TbzImm3263
mkTbzImm3263 w =
  TbzImm3263 (fromIntegral $ extract tbzImm3263ImmField w)

tbzImm3263Operand :: OperandPayload
tbzImm3263Operand =
  OperandPayload { opTypeT = [t| TbzImm3263 |]
                 , opConE  = Just (varE 'mkTbzImm3263)
                 , opWordE = Just (varE 'tbzImm3263ToBits)
                 }

data Uimm12s1 = Uimm12s1 { uimm12s1Imm :: Word16
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Uimm12s1 where
  pPrint (Uimm12s1 v) =
      PP.char '#' <> (PP.text $ show $ ((fromIntegral v) :: Word32))

instance A.Arbitrary Uimm12s1 where
  arbitrary g = Uimm12s1 <$> A.arbitrary g

uimm12s1ImmField :: Field
uimm12s1ImmField = Field 12 0

uimm12s1ToBits :: Uimm12s1 -> Word32
uimm12s1ToBits val =
  insert uimm12s1ImmField (uimm12s1Imm val) 0

mkUimm12s1 :: Word32 -> Uimm12s1
mkUimm12s1 w =
  Uimm12s1 (fromIntegral $ extract uimm12s1ImmField w)

uimm12s1Operand :: OperandPayload
uimm12s1Operand =
  OperandPayload { opTypeT = [t| Uimm12s1 |]
                 , opConE  = Just (varE 'mkUimm12s1)
                 , opWordE = Just (varE 'uimm12s1ToBits)
                 }

data Uimm12s16 = Uimm12s16 { uimm12s16Imm :: Word16
                           } deriving (Eq, Ord, Show)

instance PP.Pretty Uimm12s16 where
  pPrint _ = PP.text "Uimm12s16: not implemented"

instance A.Arbitrary Uimm12s16 where
  arbitrary g = Uimm12s16 <$> A.arbitrary g

uimm12s16ImmField :: Field
uimm12s16ImmField = Field 12 0

uimm12s16ToBits :: Uimm12s16 -> Word32
uimm12s16ToBits val =
  insert uimm12s16ImmField (uimm12s16Imm val) 0

mkUimm12s16 :: Word32 -> Uimm12s16
mkUimm12s16 w =
  Uimm12s16 (fromIntegral $ extract uimm12s16ImmField w)

uimm12s16Operand :: OperandPayload
uimm12s16Operand =
  OperandPayload { opTypeT = [t| Uimm12s16 |]
                 , opConE  = Just (varE 'mkUimm12s16)
                 , opWordE = Just (varE 'uimm12s16ToBits)
                 }

data Uimm12s2 = Uimm12s2 { uimm12s2Imm :: Word16
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Uimm12s2 where
  pPrint (Uimm12s2 v) =
      PP.char '#' <> (PP.text $ show $ ((fromIntegral v) :: Word32) `shiftL` 1)

instance A.Arbitrary Uimm12s2 where
  arbitrary g = Uimm12s2 <$> A.arbitrary g

uimm12s2ImmField :: Field
uimm12s2ImmField = Field 12 0

uimm12s2ToBits :: Uimm12s2 -> Word32
uimm12s2ToBits val =
  insert uimm12s2ImmField (uimm12s2Imm val) 0

mkUimm12s2 :: Word32 -> Uimm12s2
mkUimm12s2 w =
  Uimm12s2 (fromIntegral $ extract uimm12s2ImmField w)

uimm12s2Operand :: OperandPayload
uimm12s2Operand =
  OperandPayload { opTypeT = [t| Uimm12s2 |]
                 , opConE  = Just (varE 'mkUimm12s2)
                 , opWordE = Just (varE 'uimm12s2ToBits)
                 }

data Uimm12s4 = Uimm12s4 { uimm12s4Imm :: Word16
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Uimm12s4 where
  pPrint (Uimm12s4 v) =
      PP.char '#' <> (PP.text $ show $ ((fromIntegral v) :: Word32) `shiftL` 2)

instance A.Arbitrary Uimm12s4 where
  arbitrary g = Uimm12s4 <$> A.arbitrary g

uimm12s4ImmField :: Field
uimm12s4ImmField = Field 12 0

uimm12s4ToBits :: Uimm12s4 -> Word32
uimm12s4ToBits val =
  insert uimm12s4ImmField (uimm12s4Imm val) 0

mkUimm12s4 :: Word32 -> Uimm12s4
mkUimm12s4 w =
  Uimm12s4 (fromIntegral $ extract uimm12s4ImmField w)

uimm12s4Operand :: OperandPayload
uimm12s4Operand =
  OperandPayload { opTypeT = [t| Uimm12s4 |]
                 , opConE  = Just (varE 'mkUimm12s4)
                 , opWordE = Just (varE 'uimm12s4ToBits)
                 }

data Uimm12s8 = Uimm12s8 { uimm12s8Imm :: Word16
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Uimm12s8 where
  pPrint (Uimm12s8 v) =
      PP.char '#' <> (PP.text $ show $ ((fromIntegral v) :: Word32) `shiftL` 3)

instance A.Arbitrary Uimm12s8 where
  arbitrary g = Uimm12s8 <$> A.arbitrary g

uimm12s8ImmField :: Field
uimm12s8ImmField = Field 12 0

uimm12s8ToBits :: Uimm12s8 -> Word32
uimm12s8ToBits val =
  insert uimm12s8ImmField (uimm12s8Imm val) 0

mkUimm12s8 :: Word32 -> Uimm12s8
mkUimm12s8 w =
  Uimm12s8 (fromIntegral $ extract uimm12s8ImmField w)

uimm12s8Operand :: OperandPayload
uimm12s8Operand =
  OperandPayload { opTypeT = [t| Uimm12s8 |]
                 , opConE  = Just (varE 'mkUimm12s8)
                 , opWordE = Just (varE 'uimm12s8ToBits)
                 }

data Addext = Addext { addextImm :: Word8
                     , addextOption :: Word8
                     } deriving (Eq, Ord, Show)

instance PP.Pretty Addext where
  pPrint (Addext imm opt) =
      let s = case opt of
                0b000 -> "uxtb"
                0b001 -> "uxth"
                0b010 -> "uxtw"
                0b011 -> "uxtx"
                0b100 -> "sxtb"
                0b101 -> "sxth"
                0b110 -> "sxtw"
                0b111 -> "sxtx"
                _ -> error $ "Invalid Addext option value: " <> show opt
          immS = if imm == 0 then "" else " #" <> show imm
      in PP.text $ s <> immS

instance A.Arbitrary Addext where
  arbitrary g = Addext <$> A.arbitrary g <*> A.arbitrary g

addextImmField :: Field
addextImmField = Field 3 0

addextOptionField :: Field
addextOptionField = Field 3 3

addextToBits :: Addext -> Word32
addextToBits val =
  insert addextImmField (addextImm val) $
  insert addextOptionField (addextOption val) 0

mkAddext :: Word32 -> Addext
mkAddext w =
  Addext (fromIntegral $ extract addextImmField w)
         (fromIntegral $ extract addextOptionField w)

addextOperand :: OperandPayload
addextOperand =
  OperandPayload { opTypeT = [t| Addext |]
                 , opConE  = Just (varE 'mkAddext)
                 , opWordE = Just (varE 'addextToBits)
                 }

data FixedpointF32I32 = FixedpointF32I32 { fixedpointF32I32Scale :: Word8
                                         } deriving (Eq, Ord, Show)

instance PP.Pretty FixedpointF32I32 where
  pPrint _ = PP.text "FixedpointF32I32: not implemented"

instance A.Arbitrary FixedpointF32I32 where
  arbitrary g = FixedpointF32I32 <$> A.arbitrary g

fixedpointF32I32ScaleField :: Field
fixedpointF32I32ScaleField = Field 5 0

fixedpointF32I32ToBits :: FixedpointF32I32 -> Word32
fixedpointF32I32ToBits val =
  insert fixedpointF32I32ScaleField (fixedpointF32I32Scale val) 0

mkFixedpointF32I32 :: Word32 -> FixedpointF32I32
mkFixedpointF32I32 w =
  FixedpointF32I32 (fromIntegral $ extract fixedpointF32I32ScaleField w)

fixedpointF32I32Operand :: OperandPayload
fixedpointF32I32Operand =
  OperandPayload { opTypeT = [t| FixedpointF32I32 |]
                 , opConE  = Just (varE 'mkFixedpointF32I32)
                 , opWordE = Just (varE 'fixedpointF32I32ToBits)
                 }

data FixedpointF16I32 = FixedpointF16I32 { fixedpointF16I32Scale :: Word8
                                         } deriving (Eq, Ord, Show)

instance PP.Pretty FixedpointF16I32 where
  pPrint _ = PP.text "FixedpointF16I32: not implemented"

instance A.Arbitrary FixedpointF16I32 where
  arbitrary g = FixedpointF16I32 <$> A.arbitrary g

fixedpointF16I32ScaleField :: Field
fixedpointF16I32ScaleField = Field 5 0

fixedpointF16I32ToBits :: FixedpointF16I32 -> Word32
fixedpointF16I32ToBits val =
  insert fixedpointF16I32ScaleField (fixedpointF16I32Scale val) 0

mkFixedpointF16I32 :: Word32 -> FixedpointF16I32
mkFixedpointF16I32 w =
  FixedpointF16I32 (fromIntegral $ extract fixedpointF16I32ScaleField w)

fixedpointF16I32Operand :: OperandPayload
fixedpointF16I32Operand =
  OperandPayload { opTypeT = [t| FixedpointF16I32 |]
                 , opConE  = Just (varE 'mkFixedpointF16I32)
                 , opWordE = Just (varE 'fixedpointF16I32ToBits)
                 }

data FixedpointF16I64 = FixedpointF16I64 { fixedpointF16I64Scale :: Word8
                                         } deriving (Eq, Ord, Show)

instance PP.Pretty FixedpointF16I64 where
  pPrint _ = PP.text "FixedpointF16I64: not implemented"

instance A.Arbitrary FixedpointF16I64 where
  arbitrary g = FixedpointF16I64 <$> A.arbitrary g

fixedpointF16I64ScaleField :: Field
fixedpointF16I64ScaleField = Field 6 0

fixedpointF16I64ToBits :: FixedpointF16I64 -> Word32
fixedpointF16I64ToBits val =
  insert fixedpointF16I64ScaleField (fixedpointF16I64Scale val) 0

mkFixedpointF16I64 :: Word32 -> FixedpointF16I64
mkFixedpointF16I64 w =
  FixedpointF16I64 (fromIntegral $ extract fixedpointF16I64ScaleField w)

fixedpointF16I64Operand :: OperandPayload
fixedpointF16I64Operand =
  OperandPayload { opTypeT = [t| FixedpointF16I64 |]
                 , opConE  = Just (varE 'mkFixedpointF16I64)
                 , opWordE = Just (varE 'fixedpointF16I64ToBits)
                 }

data FixedpointF32I64 = FixedpointF32I64 { fixedpointF32I64Scale :: Word8
                                         } deriving (Eq, Ord, Show)

instance PP.Pretty FixedpointF32I64 where
  pPrint _ = PP.text "FixedpointF32I64: not implemented"

instance A.Arbitrary FixedpointF32I64 where
  arbitrary g = FixedpointF32I64 <$> A.arbitrary g

fixedpointF32I64ScaleField :: Field
fixedpointF32I64ScaleField = Field 6 0

fixedpointF32I64ToBits :: FixedpointF32I64 -> Word32
fixedpointF32I64ToBits val =
  insert fixedpointF32I64ScaleField (fixedpointF32I64Scale val) 0

mkFixedpointF32I64 :: Word32 -> FixedpointF32I64
mkFixedpointF32I64 w =
  FixedpointF32I64 (fromIntegral $ extract fixedpointF32I64ScaleField w)

fixedpointF32I64Operand :: OperandPayload
fixedpointF32I64Operand =
  OperandPayload { opTypeT = [t| FixedpointF32I64 |]
                 , opConE  = Just (varE 'mkFixedpointF32I64)
                 , opWordE = Just (varE 'fixedpointF32I64ToBits)
                 }

data FixedpointF64I32 = FixedpointF64I32 { fixedpointF64I32Scale :: Word8
                                         } deriving (Eq, Ord, Show)

instance PP.Pretty FixedpointF64I32 where
  pPrint _ = PP.text "FixedpointF64I32: not implemented"

instance A.Arbitrary FixedpointF64I32 where
  arbitrary g = FixedpointF64I32 <$> A.arbitrary g

fixedpointF64I32ScaleField :: Field
fixedpointF64I32ScaleField = Field 5 0

fixedpointF64I32ToBits :: FixedpointF64I32 -> Word32
fixedpointF64I32ToBits val =
  insert fixedpointF64I32ScaleField (fixedpointF64I32Scale val) 0

mkFixedpointF64I32 :: Word32 -> FixedpointF64I32
mkFixedpointF64I32 w =
  FixedpointF64I32 (fromIntegral $ extract fixedpointF64I32ScaleField w)

fixedpointF64I32Operand :: OperandPayload
fixedpointF64I32Operand =
  OperandPayload { opTypeT = [t| FixedpointF64I32 |]
                 , opConE  = Just (varE 'mkFixedpointF64I32)
                 , opWordE = Just (varE 'fixedpointF64I32ToBits)
                 }

data FixedpointF64I64 = FixedpointF64I64 { fixedpointF64I64Scale :: Word8
                                         } deriving (Eq, Ord, Show)

instance PP.Pretty FixedpointF64I64 where
  pPrint _ = PP.text "FixedpointF64I64: not implemented"

instance A.Arbitrary FixedpointF64I64 where
  arbitrary g = FixedpointF64I64 <$> A.arbitrary g

fixedpointF64I64ScaleField :: Field
fixedpointF64I64ScaleField = Field 6 0

fixedpointF64I64ToBits :: FixedpointF64I64 -> Word32
fixedpointF64I64ToBits val =
  insert fixedpointF64I64ScaleField (fixedpointF64I64Scale val) 0

mkFixedpointF64I64 :: Word32 -> FixedpointF64I64
mkFixedpointF64I64 w =
  FixedpointF64I64 (fromIntegral $ extract fixedpointF64I64ScaleField w)

fixedpointF64I64Operand :: OperandPayload
fixedpointF64I64Operand =
  OperandPayload { opTypeT = [t| FixedpointF64I64 |]
                 , opConE  = Just (varE 'mkFixedpointF64I64)
                 , opWordE = Just (varE 'fixedpointF64I64ToBits)
                 }

data Fpimm16 = Fpimm16 { fpimm16Imm :: Word8
                       } deriving (Eq, Ord, Show)

instance PP.Pretty Fpimm16 where
  pPrint _ = PP.text "Fpimm16: not implemented"

instance A.Arbitrary Fpimm16 where
  arbitrary g = Fpimm16 <$> A.arbitrary g

fpimm16ImmField :: Field
fpimm16ImmField = Field 8 0

fpimm16ToBits :: Fpimm16 -> Word32
fpimm16ToBits val =
  insert fpimm16ImmField (fpimm16Imm val) 0

mkFpimm16 :: Word32 -> Fpimm16
mkFpimm16 w =
  Fpimm16 (fromIntegral $ extract fpimm16ImmField w)

fpimm16Operand :: OperandPayload
fpimm16Operand =
  OperandPayload { opTypeT = [t| Fpimm16 |]
                 , opConE  = Just (varE 'mkFpimm16)
                 , opWordE = Just (varE 'fpimm16ToBits)
                 }

data Fpimm32 = Fpimm32 { fpimm32Imm :: Word8
                       } deriving (Eq, Ord, Show)

instance PP.Pretty Fpimm32 where
  pPrint _ = PP.text "Fpimm32: not implemented"

instance A.Arbitrary Fpimm32 where
  arbitrary g = Fpimm32 <$> A.arbitrary g

fpimm32ImmField :: Field
fpimm32ImmField = Field 8 0

fpimm32ToBits :: Fpimm32 -> Word32
fpimm32ToBits val =
  insert fpimm32ImmField (fpimm32Imm val) 0

mkFpimm32 :: Word32 -> Fpimm32
mkFpimm32 w =
  Fpimm32 (fromIntegral $ extract fpimm32ImmField w)

fpimm32Operand :: OperandPayload
fpimm32Operand =
  OperandPayload { opTypeT = [t| Fpimm32 |]
                 , opConE  = Just (varE 'mkFpimm32)
                 , opWordE = Just (varE 'fpimm32ToBits)
                 }

data Fpimm64 = Fpimm64 { fpimm64Imm :: Word8
                       } deriving (Eq, Ord, Show)

instance PP.Pretty Fpimm64 where
  pPrint _ = PP.text "Fpimm64: not implemented"

instance A.Arbitrary Fpimm64 where
  arbitrary g = Fpimm64 <$> A.arbitrary g

fpimm64ImmField :: Field
fpimm64ImmField = Field 8 0

fpimm64ToBits :: Fpimm64 -> Word32
fpimm64ToBits val =
  insert fpimm64ImmField (fpimm64Imm val) 0

mkFpimm64 :: Word32 -> Fpimm64
mkFpimm64 w =
  Fpimm64 (fromIntegral $ extract fpimm64ImmField w)

fpimm64Operand :: OperandPayload
fpimm64Operand =
  OperandPayload { opTypeT = [t| Fpimm64 |]
                 , opConE  = Just (varE 'mkFpimm64)
                 , opWordE = Just (varE 'fpimm64ToBits)
                 }

