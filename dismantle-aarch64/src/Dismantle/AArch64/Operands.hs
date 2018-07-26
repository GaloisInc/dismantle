{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
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

  , V64
  , mkV64
  , v64ToBits
  , v64Operand

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

  , Imm031b
  , mkImm031b
  , imm031bToBits
  , imm031bOperand

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

  , Fpimm8
  , mkFpimm8
  , fpimm8ToBits
  , fpimm8Operand

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

  , Addshift64
  , mkAddshift64
  , addshift64ToBits
  , addshift64Operand

  , VecListFourb
  , mkVecListFourb
  , vecListFourbToBits
  , vecListFourbOperand

  , VecListFourd
  , mkVecListFourd
  , vecListFourdToBits
  , vecListFourdOperand

  , VecListFourh
  , mkVecListFourh
  , vecListFourhToBits
  , vecListFourhOperand

  , VecListFours
  , mkVecListFours
  , vecListFoursToBits
  , vecListFoursOperand

  , VecListOneb
  , mkVecListOneb
  , vecListOnebToBits
  , vecListOnebOperand

  , VecListOned
  , mkVecListOned
  , vecListOnedToBits
  , vecListOnedOperand

  , VecListOneh
  , mkVecListOneh
  , vecListOnehToBits
  , vecListOnehOperand

  , VecListOnes
  , mkVecListOnes
  , vecListOnesToBits
  , vecListOnesOperand

  , VecListThreeb
  , mkVecListThreeb
  , vecListThreebToBits
  , vecListThreebOperand

  , VecListThreed
  , mkVecListThreed
  , vecListThreedToBits
  , vecListThreedOperand

  , VecListThreeh
  , mkVecListThreeh
  , vecListThreehToBits
  , vecListThreehOperand

  , VecListThrees
  , mkVecListThrees
  , vecListThreesToBits
  , vecListThreesOperand

  , VecListTwob
  , mkVecListTwob
  , vecListTwobToBits
  , vecListTwobOperand

  , VecListTwod
  , mkVecListTwod
  , vecListTwodToBits
  , vecListTwodOperand

  , VecListTwos
  , mkVecListTwos
  , vecListTwosToBits
  , vecListTwosOperand

  , VecListTwoh
  , mkVecListTwoh
  , vecListTwohToBits
  , vecListTwohOperand

  , VectorIndexS
  , mkVectorIndexS
  , vectorIndexSToBits
  , vectorIndexSOperand

  , VectorIndexD
  , mkVectorIndexD
  , vectorIndexDToBits
  , vectorIndexDOperand

  , VectorIndexB
  , mkVectorIndexB
  , vectorIndexBToBits
  , vectorIndexBOperand

  , VectorIndexH
  , mkVectorIndexH
  , vectorIndexHToBits
  , vectorIndexHOperand

  , Simdimmtype10
  , mkSimdimmtype10
  , simdimmtype10ToBits
  , simdimmtype10Operand

  , VecshiftL16
  , mkVecshiftL16
  , vecshiftL16ToBits
  , vecshiftL16Operand

  , VecshiftL8
  , mkVecshiftL8
  , vecshiftL8ToBits
  , vecshiftL8Operand

  , VecshiftL32
  , mkVecshiftL32
  , vecshiftL32ToBits
  , vecshiftL32Operand

  , VecshiftL64
  , mkVecshiftL64
  , vecshiftL64ToBits
  , vecshiftL64Operand

  , VecshiftR16
  , mkVecshiftR16
  , vecshiftR16ToBits
  , vecshiftR16Operand

  , VecshiftR8
  , mkVecshiftR8
  , vecshiftR8ToBits
  , vecshiftR8Operand

  , VecshiftR32
  , mkVecshiftR32
  , vecshiftR32ToBits
  , vecshiftR32Operand

  , VecshiftR64
  , mkVecshiftR64
  , vecshiftR64ToBits
  , vecshiftR64Operand

  , I32imm
  , mkI32imm
  , i32immToBits
  , i32immOperand

  , Imm0255
  , mkImm0255
  , imm0255ToBits
  , imm0255Operand

  , LogicalVecHwShift
  , mkLogicalVecHwShift
  , logicalVecHwShiftToBits
  , logicalVecHwShiftOperand

  , LogicalVecShift
  , mkLogicalVecShift
  , logicalVecShiftToBits
  , logicalVecShiftOperand

  , MoveVecShift
  , mkMoveVecShift
  , moveVecShiftToBits
  , moveVecShiftOperand

  , VecshiftR16Narrow
  , mkVecshiftR16Narrow
  , vecshiftR16NarrowToBits
  , vecshiftR16NarrowOperand

  , VecshiftR32Narrow
  , mkVecshiftR32Narrow
  , vecshiftR32NarrowToBits
  , vecshiftR32NarrowOperand

  , VecshiftR64Narrow
  , mkVecshiftR64Narrow
  , vecshiftR64NarrowToBits
  , vecshiftR64NarrowOperand

  )
where

import GHC.TypeLits
import Data.Bits
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Int ( Int16, Int32, Int64, Int8 )
import qualified Data.Word.Indexed as W
import qualified Data.Parameterized.NatRepr as NR

import Numeric (showHex)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Text.PrettyPrint.HughesPJClass as PP

import Dismantle.Tablegen.ISA
import qualified Dismantle.Arbitrary as A
import Dismantle.Tablegen.TH.Pretty

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

-- Operand types

data FPR128 = FPR128 { fPR128Reg :: W.W 5
                     } deriving (Eq, Ord, Show)

instance PP.Pretty FPR128 where
  pPrint (FPR128 r) = PP.text $ "q" <> show r

instance A.Arbitrary FPR128 where
  arbitrary g = FPR128 <$> A.arbitrary g

fPR128RegField :: Field 5
fPR128RegField = field 0

fPR128ToBits :: FPR128 -> Word32
fPR128ToBits val =
  insert fPR128RegField (fPR128Reg val) 0

mkFPR128 :: Word32 -> FPR128
mkFPR128 w =
  FPR128 (extract fPR128RegField w)

fPR128Operand :: OperandPayload
fPR128Operand =
  OperandPayload { opTypeT = [t| FPR128 |]
                 , opConE  = Just (varE 'mkFPR128)
                 , opWordE = Just (varE 'fPR128ToBits)
                 }

data FPR16 = FPR16 { fPR16Reg :: W.W 5
                     } deriving (Eq, Ord, Show)

instance PP.Pretty FPR16 where
  pPrint (FPR16 r) = PP.text $ "h" <> show r

instance A.Arbitrary FPR16 where
  arbitrary g = FPR16 <$> A.arbitrary g

fPR16RegField :: Field 5
fPR16RegField = field 0

fPR16ToBits :: FPR16 -> Word32
fPR16ToBits val =
  insert fPR16RegField (fPR16Reg val) 0

mkFPR16 :: Word32 -> FPR16
mkFPR16 w =
  FPR16 (extract fPR16RegField w)

fPR16Operand :: OperandPayload
fPR16Operand =
  OperandPayload { opTypeT = [t| FPR16 |]
                 , opConE  = Just (varE 'mkFPR16)
                 , opWordE = Just (varE 'fPR16ToBits)
                 }

data FPR32 = FPR32 { fPR32Reg :: W.W 5
                     } deriving (Eq, Ord, Show)

instance PP.Pretty FPR32 where
  pPrint (FPR32 r) = PP.text $ "s" <> show r

instance A.Arbitrary FPR32 where
  arbitrary g = FPR32 <$> A.arbitrary g

fPR32RegField :: Field 5
fPR32RegField = field 0

fPR32ToBits :: FPR32 -> Word32
fPR32ToBits val =
  insert fPR32RegField (fPR32Reg val) 0

mkFPR32 :: Word32 -> FPR32
mkFPR32 w =
  FPR32 (extract fPR32RegField w)

fPR32Operand :: OperandPayload
fPR32Operand =
  OperandPayload { opTypeT = [t| FPR32 |]
                 , opConE  = Just (varE 'mkFPR32)
                 , opWordE = Just (varE 'fPR32ToBits)
                 }

data FPR64 = FPR64 { fPR64Reg :: W.W 5
                   } deriving (Eq, Ord, Show)

instance PP.Pretty FPR64 where
  pPrint (FPR64 r) = PP.text $ "d" <> show r

instance A.Arbitrary FPR64 where
  arbitrary g = FPR64 <$> A.arbitrary g

fPR64RegField :: Field 5
fPR64RegField = field 0

fPR64ToBits :: FPR64 -> Word32
fPR64ToBits val =
  insert fPR64RegField (fPR64Reg val) 0

mkFPR64 :: Word32 -> FPR64
mkFPR64 w =
  FPR64 (extract fPR64RegField w)

fPR64Operand :: OperandPayload
fPR64Operand =
  OperandPayload { opTypeT = [t| FPR64 |]
                 , opConE  = Just (varE 'mkFPR64)
                 , opWordE = Just (varE 'fPR64ToBits)
                 }

data FPR8 = FPR8 { fPR8Reg :: W.W 5
                 } deriving (Eq, Ord, Show)

instance PP.Pretty FPR8 where
  pPrint (FPR8 r) = PP.text $ "b" <> show r

instance A.Arbitrary FPR8 where
  arbitrary g = FPR8 <$> A.arbitrary g

fPR8RegField :: Field 5
fPR8RegField = field 0

fPR8ToBits :: FPR8 -> Word32
fPR8ToBits val =
  insert fPR8RegField (fPR8Reg val) 0

mkFPR8 :: Word32 -> FPR8
mkFPR8 w =
  FPR8 (extract fPR8RegField w)

fPR8Operand :: OperandPayload
fPR8Operand =
  OperandPayload { opTypeT = [t| FPR8 |]
                 , opConE  = Just (varE 'mkFPR8)
                 , opWordE = Just (varE 'fPR8ToBits)
                 }

data GPR32 = GPR32 { gPR32Reg :: W.W 5
                   } deriving (Eq, Ord, Show)

instance PP.Pretty GPR32 where
  pPrint (GPR32 31) = PP.text "wzr"
  pPrint (GPR32 r) = PP.text $ "w" <> show r

instance A.Arbitrary GPR32 where
  arbitrary g = GPR32 <$> A.arbitrary g

gPR32RegField :: Field 5
gPR32RegField = field 0

gPR32ToBits :: GPR32 -> Word32
gPR32ToBits val =
  insert gPR32RegField (gPR32Reg val) 0

mkGPR32 :: Word32 -> GPR32
mkGPR32 w =
  GPR32 (extract gPR32RegField w)

gPR32Operand :: OperandPayload
gPR32Operand =
  OperandPayload { opTypeT = [t| GPR32 |]
                 , opConE  = Just (varE 'mkGPR32)
                 , opWordE = Just (varE 'gPR32ToBits)
                 }

data GPR64 = GPR64 { gPR64Reg :: W.W 5
                   } deriving (Eq, Ord, Show)

instance PP.Pretty GPR64 where
  pPrint (GPR64 31) = PP.text "xzr"
  pPrint (GPR64 r) = PP.text $ "x" <> show r

instance A.Arbitrary GPR64 where
  arbitrary g = GPR64 <$> A.arbitrary g

gPR64RegField :: Field 5
gPR64RegField = field 0

gPR64ToBits :: GPR64 -> Word32
gPR64ToBits val =
  insert gPR64RegField (gPR64Reg val) 0

mkGPR64 :: Word32 -> GPR64
mkGPR64 w =
  GPR64 (extract gPR64RegField w)

gPR64Operand :: OperandPayload
gPR64Operand =
  OperandPayload { opTypeT = [t| GPR64 |]
                 , opConE  = Just (varE 'mkGPR64)
                 , opWordE = Just (varE 'gPR64ToBits)
                 }

data GPR32sp = GPR32sp { gPR32spReg :: W.W 5
                       } deriving (Eq, Ord, Show)

instance PP.Pretty GPR32sp where
  pPrint (GPR32sp 0b11111) = PP.text "wsp"
  pPrint (GPR32sp r) = PP.text $ "w" <> show r

instance A.Arbitrary GPR32sp where
  arbitrary g = GPR32sp <$> A.arbitrary g

gPR32spRegField :: Field 5
gPR32spRegField = field 0

gPR32spToBits :: GPR32sp -> Word32
gPR32spToBits val =
  insert gPR32spRegField (gPR32spReg val) 0

mkGPR32sp :: Word32 -> GPR32sp
mkGPR32sp w =
  GPR32sp (extract gPR32spRegField w)

gPR32spOperand :: OperandPayload
gPR32spOperand =
  OperandPayload { opTypeT = [t| GPR32sp |]
                 , opConE  = Just (varE 'mkGPR32sp)
                 , opWordE = Just (varE 'gPR32spToBits)
                 }

data GPR64sp = GPR64sp { gPR64spReg :: W.W 5
                       } deriving (Eq, Ord, Show)

instance PP.Pretty GPR64sp where
  pPrint (GPR64sp 0b11111) = PP.text "sp"
  pPrint (GPR64sp r) = PP.text $ "x" <> show r

instance A.Arbitrary GPR64sp where
  arbitrary g = GPR64sp <$> A.arbitrary g

gPR64spRegField :: Field 5
gPR64spRegField = field 0

gPR64spToBits :: GPR64sp -> Word32
gPR64spToBits val =
  insert gPR64spRegField (gPR64spReg val) 0

mkGPR64sp :: Word32 -> GPR64sp
mkGPR64sp w =
  GPR64sp $ (extract gPR64spRegField w)

gPR64spOperand :: OperandPayload
gPR64spOperand =
  OperandPayload { opTypeT = [t| GPR64sp |]
                 , opConE  = Just (varE 'mkGPR64sp)
                 , opWordE = Just (varE 'gPR64spToBits)
                 }

data V128 = V128 { v128Reg :: W.W 5
                 } deriving (Eq, Ord, Show)

instance PP.Pretty V128 where
  pPrint (V128 r) = PP.text $ "v" <> show r

instance A.Arbitrary V128 where
  arbitrary g = V128 <$> A.arbitrary g

v128RegField :: Field 5
v128RegField = field 0

v128ToBits :: V128 -> Word32
v128ToBits val =
  insert v128RegField (v128Reg val) 0

mkV128 :: Word32 -> V128
mkV128 w =
  V128 $ (extract v128RegField w)

v128Operand :: OperandPayload
v128Operand =
  OperandPayload { opTypeT = [t| V128 |]
                 , opConE  = Just (varE 'mkV128)
                 , opWordE = Just (varE 'v128ToBits)
                 }

data AddsubShiftedImm32 = AddsubShiftedImm32 { addsubShiftedImm32Imm :: W.W 12
                                             , addsubShiftedImm32Shift :: W.W 2
                                             } deriving (Eq, Ord, Show)

instance PP.Pretty AddsubShiftedImm32 where
  pPrint (AddsubShiftedImm32 imm shift) =
      let i = PP.text $ case shift of
                0b0 -> ""
                0b1 -> ", lsl #12"
                _ -> error $ "Invalid AddsubShiftedImm32 value: " <> show shift
      in PP.text ("#0x" <> showHex (W.unW imm) "") <> i

instance A.Arbitrary AddsubShiftedImm32 where
  arbitrary g = AddsubShiftedImm32 <$> A.arbitrary g <*> A.arbitrary g

addsubShiftedImm32ImmField :: Field 12
addsubShiftedImm32ImmField = field 0

addsubShiftedImm32ShiftField :: Field 2
addsubShiftedImm32ShiftField = field 12

addsubShiftedImm32ToBits :: AddsubShiftedImm32 -> Word32
addsubShiftedImm32ToBits val =
  insert addsubShiftedImm32ImmField (addsubShiftedImm32Imm val) $
  insert addsubShiftedImm32ShiftField (addsubShiftedImm32Shift val) 0

mkAddsubShiftedImm32 :: Word32 -> AddsubShiftedImm32
mkAddsubShiftedImm32 w =
  AddsubShiftedImm32 (extract addsubShiftedImm32ImmField w)
                     (extract addsubShiftedImm32ShiftField w)

addsubShiftedImm32Operand :: OperandPayload
addsubShiftedImm32Operand =
  OperandPayload { opTypeT = [t| AddsubShiftedImm32 |]
                 , opConE  = Just (varE 'mkAddsubShiftedImm32)
                 , opWordE = Just (varE 'addsubShiftedImm32ToBits)
                 }

data AddsubShiftedImm64 = AddsubShiftedImm64 { addsubShiftedImm64Imm :: W.W 12
                                             , addsubShiftedImm64Shift :: W.W 2
                                             } deriving (Eq, Ord, Show)

instance PP.Pretty AddsubShiftedImm64 where
  pPrint (AddsubShiftedImm64 imm shift) =
      let shiftStr = case shift of
                0b0 -> mempty -- Default lsl #0, omit
                0b1 -> PP.text ", lsl #12"
                _ -> error $ "invalid AddsubShiftedImm64 value: " <> show shift
      in (PP.text "#0x" <> PP.text (showHex (W.unW imm) "")) <> shiftStr

instance A.Arbitrary AddsubShiftedImm64 where
  arbitrary g = AddsubShiftedImm64 <$> A.arbitrary g <*> A.arbitrary g

addsubShiftedImm64ImmField :: Field 12
addsubShiftedImm64ImmField = field 0

addsubShiftedImm64ShiftField :: Field 2
addsubShiftedImm64ShiftField = field 12

addsubShiftedImm64ToBits :: AddsubShiftedImm64 -> Word32
addsubShiftedImm64ToBits val =
  insert addsubShiftedImm64ImmField (addsubShiftedImm64Imm val) $
  insert addsubShiftedImm64ShiftField (addsubShiftedImm64Shift val) 0

mkAddsubShiftedImm64 :: Word32 -> AddsubShiftedImm64
mkAddsubShiftedImm64 w =
  AddsubShiftedImm64 (extract addsubShiftedImm64ImmField w)
                     (extract addsubShiftedImm64ShiftField w)

addsubShiftedImm64Operand :: OperandPayload
addsubShiftedImm64Operand =
  OperandPayload { opTypeT = [t| AddsubShiftedImm64 |]
                 , opConE  = Just (varE 'mkAddsubShiftedImm64)
                 , opWordE = Just (varE 'addsubShiftedImm64ToBits)
                 }

data Adrlabel = Adrlabel { adrlabelImm :: W.W 21
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Adrlabel where
  pPrint _ = PP.text "Adrlabel: not implemented"

instance A.Arbitrary Adrlabel where
  arbitrary g = Adrlabel <$> A.arbitrary g

adrlabelImmField :: Field 21
adrlabelImmField = field 0

adrlabelToBits :: Adrlabel -> Word32
adrlabelToBits val =
  insert adrlabelImmField (adrlabelImm val) 0

mkAdrlabel :: Word32 -> Adrlabel
mkAdrlabel w =
  Adrlabel (extract adrlabelImmField w)

adrlabelOperand :: OperandPayload
adrlabelOperand =
  OperandPayload { opTypeT = [t| Adrlabel |]
                 , opConE  = Just (varE 'mkAdrlabel)
                 , opWordE = Just (varE 'adrlabelToBits)
                 }

data Adrplabel = Adrplabel { adrplabelImm :: W.W 21
                           } deriving (Eq, Ord, Show)

instance PP.Pretty Adrplabel where
  pPrint _ = PP.text "Adrplabel: not implemented"

instance A.Arbitrary Adrplabel where
  arbitrary g = Adrplabel <$> A.arbitrary g

adrplabelImmField :: Field 21
adrplabelImmField = field 0

adrplabelToBits :: Adrplabel -> Word32
adrplabelToBits val =
  insert adrplabelImmField (adrplabelImm val) 0

mkAdrplabel :: Word32 -> Adrplabel
mkAdrplabel w =
  Adrplabel (extract adrplabelImmField w)

adrplabelOperand :: OperandPayload
adrplabelOperand =
  OperandPayload { opTypeT = [t| Adrplabel |]
                 , opConE  = Just (varE 'mkAdrplabel)
                 , opWordE = Just (varE 'adrplabelToBits)
                 }

data AmBTarget = AmBTarget { amBTargetAddr :: W.W 26
                           } deriving (Eq, Ord, Show)

instance PP.Pretty AmBTarget where
  pPrint _ = PP.text "AmBTarget: not implemented"

instance A.Arbitrary AmBTarget where
  arbitrary g = AmBTarget <$> A.arbitrary g

amBTargetAddrField :: Field 26
amBTargetAddrField = field 0

amBTargetToBits :: AmBTarget -> Word32
amBTargetToBits val =
  insert amBTargetAddrField (amBTargetAddr val) 0

mkAmBTarget :: Word32 -> AmBTarget
mkAmBTarget w =
  AmBTarget (extract amBTargetAddrField w)

amBTargetOperand :: OperandPayload
amBTargetOperand =
  OperandPayload { opTypeT = [t| AmBTarget |]
                 , opConE  = Just (varE 'mkAmBTarget)
                 , opWordE = Just (varE 'amBTargetToBits)
                 }

data AmBrcond = AmBrcond { amBrcondAddr :: W.W 19
                         } deriving (Eq, Ord, Show)

instance PP.Pretty AmBrcond where
  pPrint _ = PP.text "AmBrcond: not implemented"

instance A.Arbitrary AmBrcond where
  arbitrary g = AmBrcond <$> A.arbitrary g

amBrcondAddrField :: Field 19
amBrcondAddrField = field 0

amBrcondToBits :: AmBrcond -> Word32
amBrcondToBits val =
  insert amBrcondAddrField (amBrcondAddr val) 0

mkAmBrcond :: Word32 -> AmBrcond
mkAmBrcond w =
  AmBrcond (extract amBrcondAddrField w)

amBrcondOperand :: OperandPayload
amBrcondOperand =
  OperandPayload { opTypeT = [t| AmBrcond |]
                 , opConE  = Just (varE 'mkAmBrcond)
                 , opWordE = Just (varE 'amBrcondToBits)
                 }

data AmLdrlit = AmLdrlit { amLdrlitLabel :: W.W 19
                         } deriving (Eq, Ord, Show)

instance PP.Pretty AmLdrlit where
  pPrint _ = PP.text "AmLdrlit: not implemented"

instance A.Arbitrary AmLdrlit where
  arbitrary g = AmLdrlit <$> A.arbitrary g

amLdrlitLabelField :: Field 19
amLdrlitLabelField = field 0

amLdrlitToBits :: AmLdrlit -> Word32
amLdrlitToBits val =
  insert amLdrlitLabelField (amLdrlitLabel val) 0

mkAmLdrlit :: Word32 -> AmLdrlit
mkAmLdrlit w =
  AmLdrlit (extract amLdrlitLabelField w)

amLdrlitOperand :: OperandPayload
amLdrlitOperand =
  OperandPayload { opTypeT = [t| AmLdrlit |]
                 , opConE  = Just (varE 'mkAmLdrlit)
                 , opWordE = Just (varE 'amLdrlitToBits)
                 }

data AmTbrcond = AmTbrcond { amTbrcondLabel :: W.W 14
                           } deriving (Eq, Ord, Show)

instance PP.Pretty AmTbrcond where
  pPrint _ = PP.text "AmTbrcond: not implemented"

instance A.Arbitrary AmTbrcond where
  arbitrary g = AmTbrcond <$> A.arbitrary g

amTbrcondLabelField :: Field 14
amTbrcondLabelField = field 0

amTbrcondToBits :: AmTbrcond -> Word32
amTbrcondToBits val =
  insert amTbrcondLabelField (amTbrcondLabel val) 0

mkAmTbrcond :: Word32 -> AmTbrcond
mkAmTbrcond w =
  AmTbrcond (extract amTbrcondLabelField w)

amTbrcondOperand :: OperandPayload
amTbrcondOperand =
  OperandPayload { opTypeT = [t| AmTbrcond |]
                 , opConE  = Just (varE 'mkAmTbrcond)
                 , opWordE = Just (varE 'amTbrcondToBits)
                 }

data ArithExtendlsl64 = ArithExtendlsl64 { arithExtendlsl64Shift :: W.W 3
                                         } deriving (Eq, Ord, Show)

instance PP.Pretty ArithExtendlsl64 where
  pPrint (ArithExtendlsl64 s) =
      if s == 0
      then mempty
      else PP.text $ ", lsl " <> (show s)

instance A.Arbitrary ArithExtendlsl64 where
  arbitrary g = ArithExtendlsl64 <$> A.arbitrary g

arithExtendlsl64ShiftField :: Field 3
arithExtendlsl64ShiftField = field 0

arithExtendlsl64ToBits :: ArithExtendlsl64 -> Word32
arithExtendlsl64ToBits val =
  insert arithExtendlsl64ShiftField (arithExtendlsl64Shift val) 0

mkArithExtendlsl64 :: Word32 -> ArithExtendlsl64
mkArithExtendlsl64 w =
  ArithExtendlsl64 (extract arithExtendlsl64ShiftField w)

arithExtendlsl64Operand :: OperandPayload
arithExtendlsl64Operand =
  OperandPayload { opTypeT = [t| ArithExtendlsl64 |]
                 , opConE  = Just (varE 'mkArithExtendlsl64)
                 , opWordE = Just (varE 'arithExtendlsl64ToBits)
                 }

data BarrierOp = BarrierOp { barrierOpType :: W.W 4
                           } deriving (Eq, Ord, Show)

instance PP.Pretty BarrierOp where
  pPrint _ = PP.text "BarrierOp: not implemented"

instance A.Arbitrary BarrierOp where
  arbitrary g = BarrierOp <$> A.arbitrary g

barrierOpTypeField :: Field 4
barrierOpTypeField = field 0

barrierOpToBits :: BarrierOp -> Word32
barrierOpToBits val =
  insert barrierOpTypeField (barrierOpType val) 0

mkBarrierOp :: Word32 -> BarrierOp
mkBarrierOp w =
  BarrierOp (extract barrierOpTypeField w)

barrierOpOperand :: OperandPayload
barrierOpOperand =
  OperandPayload { opTypeT = [t| BarrierOp |]
                 , opConE  = Just (varE 'mkBarrierOp)
                 , opWordE = Just (varE 'barrierOpToBits)
                 }

data Imm01 = Imm01 { imm01Bit :: W.W 1
                   } deriving (Eq, Ord, Show)

instance PP.Pretty Imm01 where
  pPrint _ = PP.text "Imm01: not implemented"

instance A.Arbitrary Imm01 where
  arbitrary g = Imm01 <$> A.arbitrary g

imm01BitField :: Field 1
imm01BitField = field 0

imm01ToBits :: Imm01 -> Word32
imm01ToBits val =
  insert imm01BitField (imm01Bit val) 0

mkImm01 :: Word32 -> Imm01
mkImm01 w =
  Imm01 (extract imm01BitField w)

imm01Operand :: OperandPayload
imm01Operand =
  OperandPayload { opTypeT = [t| Imm01 |]
                 , opConE  = Just (varE 'mkImm01)
                 , opWordE = Just (varE 'imm01ToBits)
                 }

data Imm0127 = Imm0127 { imm0127Imm :: W.W 7
                       } deriving (Eq, Ord, Show)

instance PP.Pretty Imm0127 where
  pPrint _ = PP.text "Imm0127: not implemented"

instance A.Arbitrary Imm0127 where
  arbitrary g = Imm0127 <$> A.arbitrary g

imm0127ImmField :: Field 7
imm0127ImmField = field 0

imm0127ToBits :: Imm0127 -> Word32
imm0127ToBits val =
  insert imm0127ImmField (imm0127Imm val) 0

mkImm0127 :: Word32 -> Imm0127
mkImm0127 w =
  Imm0127 (extract imm0127ImmField w)

imm0127Operand :: OperandPayload
imm0127Operand =
  OperandPayload { opTypeT = [t| Imm0127 |]
                 , opConE  = Just (varE 'mkImm0127)
                 , opWordE = Just (varE 'imm0127ToBits)
                 }

data Imm015 = Imm015 { imm015Imm :: W.W 4
                     } deriving (Eq, Ord, Show)

instance PP.Pretty Imm015 where
  pPrint _ = PP.text "Imm015: not implemented"

instance A.Arbitrary Imm015 where
  arbitrary g = Imm015 <$> A.arbitrary g

imm015ImmField :: Field 4
imm015ImmField = field 0

imm015ToBits :: Imm015 -> Word32
imm015ToBits val =
  insert imm015ImmField (imm015Imm val) 0

mkImm015 :: Word32 -> Imm015
mkImm015 w =
  Imm015 (extract imm015ImmField w)

imm015Operand :: OperandPayload
imm015Operand =
  OperandPayload { opTypeT = [t| Imm015 |]
                 , opConE  = Just (varE 'mkImm015)
                 , opWordE = Just (varE 'imm015ToBits)
                 }

data Imm031b = Imm031b { imm031bImm :: W.W 5
                     } deriving (Eq, Ord, Show)

instance PP.Pretty Imm031b where
  pPrint (Imm031b i) = PP.text $ "#" <> show i

instance A.Arbitrary Imm031b where
  arbitrary g = Imm031b <$> A.arbitrary g

imm031bImmField :: Field 5
imm031bImmField = field 0

imm031bToBits :: Imm031b -> Word32
imm031bToBits val =
  insert imm031bImmField (imm031bImm val) 0

mkImm031b :: Word32 -> Imm031b
mkImm031b w =
  Imm031b (extract imm031bImmField w)

imm031bOperand :: OperandPayload
imm031bOperand =
  OperandPayload { opTypeT = [t| Imm031b |]
                 , opConE  = Just (varE 'mkImm031b)
                 , opWordE = Just (varE 'imm031bToBits)
                 }

data Imm031 = Imm031 { imm031Imm :: W.W 5
                     } deriving (Eq, Ord, Show)

instance PP.Pretty Imm031 where
  pPrint (Imm031 i) = PP.text $ "#0x" <> showHex (W.unW i) ""

instance A.Arbitrary Imm031 where
  arbitrary g = Imm031 <$> A.arbitrary g

imm031ImmField :: Field 5
imm031ImmField = field 0

imm031ToBits :: Imm031 -> Word32
imm031ToBits val =
  insert imm031ImmField (imm031Imm val) 0

mkImm031 :: Word32 -> Imm031
mkImm031 w =
  Imm031 (extract imm031ImmField w)

imm031Operand :: OperandPayload
imm031Operand =
  OperandPayload { opTypeT = [t| Imm031 |]
                 , opConE  = Just (varE 'mkImm031)
                 , opWordE = Just (varE 'imm031ToBits)
                 }

data Imm063 = Imm063 { imm063Imm :: W.W 6
                     } deriving (Eq, Ord, Show)

instance PP.Pretty Imm063 where
  pPrint (Imm063 i) = PP.char '#' <> (PP.text $ show i)

instance A.Arbitrary Imm063 where
  arbitrary g = Imm063 <$> A.arbitrary g

imm063ImmField :: Field 6
imm063ImmField = field 0

imm063ToBits :: Imm063 -> Word32
imm063ToBits val =
  insert imm063ImmField (imm063Imm val) 0

mkImm063 :: Word32 -> Imm063
mkImm063 w =
  Imm063 (extract imm063ImmField w)

imm063Operand :: OperandPayload
imm063Operand =
  OperandPayload { opTypeT = [t| Imm063 |]
                 , opConE  = Just (varE 'mkImm063)
                 , opWordE = Just (varE 'imm063ToBits)
                 }

data Imm065535 = Imm065535 { imm065535Imm :: W.W 16
                           } deriving (Eq, Ord, Show)

instance PP.Pretty Imm065535 where
  pPrint (Imm065535 v) = PP.text $ "#0x" <> showHex (W.unW v) ""

instance A.Arbitrary Imm065535 where
  arbitrary g = Imm065535 <$> A.arbitrary g

imm065535ImmField :: Field 16
imm065535ImmField = field 0

imm065535ToBits :: Imm065535 -> Word32
imm065535ToBits val =
  insert imm065535ImmField (imm065535Imm val) 0

mkImm065535 :: Word32 -> Imm065535
mkImm065535 w =
  Imm065535 (extract imm065535ImmField w)

imm065535Operand :: OperandPayload
imm065535Operand =
  OperandPayload { opTypeT = [t| Imm065535 |]
                 , opConE  = Just (varE 'mkImm065535)
                 , opWordE = Just (varE 'imm065535ToBits)
                 }

data Imm07 = Imm07 { imm07Imm :: W.W 3
                   } deriving (Eq, Ord, Show)

instance PP.Pretty Imm07 where
  pPrint _ = PP.text "Imm07: not implemented"

instance A.Arbitrary Imm07 where
  arbitrary g = Imm07 <$> A.arbitrary g

imm07ImmField :: Field 3
imm07ImmField = field 0

imm07ToBits :: Imm07 -> Word32
imm07ToBits val =
  insert imm07ImmField (imm07Imm val) 0

mkImm07 :: Word32 -> Imm07
mkImm07 w =
  Imm07 (extract imm07ImmField w)

imm07Operand :: OperandPayload
imm07Operand =
  OperandPayload { opTypeT = [t| Imm07 |]
                 , opConE  = Just (varE 'mkImm07)
                 , opWordE = Just (varE 'imm07ToBits)
                 }

data Imm32015 = Imm32015 { imm32015Nzcv :: W.W 4
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Imm32015 where
  pPrint (Imm32015 v) =
      PP.text $ "#0x" <> showHex (W.unW v) ""

instance A.Arbitrary Imm32015 where
  arbitrary g = Imm32015 <$> A.arbitrary g

imm32015NzcvField :: Field 4
imm32015NzcvField = field 0

imm32015ToBits :: Imm32015 -> Word32
imm32015ToBits val =
  insert imm32015NzcvField (imm32015Nzcv val) 0

mkImm32015 :: Word32 -> Imm32015
mkImm32015 w =
  Imm32015 (extract imm32015NzcvField w)

imm32015Operand :: OperandPayload
imm32015Operand =
  OperandPayload { opTypeT = [t| Imm32015 |]
                 , opConE  = Just (varE 'mkImm32015)
                 , opWordE = Just (varE 'imm32015ToBits)
                 }

data Imm32031 = Imm32031 { imm32031Imm :: W.W 5
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Imm32031 where
  pPrint (Imm32031 v) = PP.text $ "#0x" <> showHex (W.unW v) ""

instance A.Arbitrary Imm32031 where
  arbitrary g = Imm32031 <$> A.arbitrary g

imm32031ImmField :: Field 5
imm32031ImmField = field 0

imm32031ToBits :: Imm32031 -> Word32
imm32031ToBits val =
  insert imm32031ImmField (imm32031Imm val) 0

mkImm32031 :: Word32 -> Imm32031
mkImm32031 w =
  Imm32031 (extract imm32031ImmField w)

imm32031Operand :: OperandPayload
imm32031Operand =
  OperandPayload { opTypeT = [t| Imm32031 |]
                 , opConE  = Just (varE 'mkImm32031)
                 , opWordE = Just (varE 'imm32031ToBits)
                 }

data LogicalImm32 = LogicalImm32 { logicalImm32Imms :: W.W 6
                                 , logicalImm32Immr :: W.W 6
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty LogicalImm32 where
  pPrint (LogicalImm32 s r) =
      let (imm, _) = decodeBitMasks 32 0 (fromIntegral $ W.unW s) (fromIntegral $ W.unW r) True
      in PP.text $ "#0x" <> showHex imm ""

-- See ARM DDI 0487A.a, AppxG-4907, "aarch64/instrs/integer/bitmasks"
decodeBitMasks :: Int -> Word64 -> Word64 -> Word64 -> Bool -> (Word64, Word64)
decodeBitMasks nBits immN imms immr immediate =
    -- Compute log2 of element size
    -- 2^len must be in range [2, M]
    let len = highestSetBit $ (immN `shiftL` 6) .|. ((complement imms) .&. (ones 6))

        -- Determine S, R and S - R parameters
        levels = ones len

        -- For logical immediates an all-ones value of S is reserved
        -- since it would generate a useless all-ones result (many times)
        -- if immediate && (imms .&. levels) == levels then
        --     ReservedValue();

        s = imms .&. levels
        r = immr .&. levels
        diff = s - r -- 6-bit subtract with borrow

        esize = 1 `shiftL` len
        d = diff .&. ones len
        welem = ones $ fromIntegral $ s + 1
        telem = ones $ fromIntegral $ d + 1
        wmask = replicateBits nBits esize welem'
        welem' = fromIntegral $ rotateRight esize (fromIntegral r) welem
        tmask = replicateBits nBits esize telem
    in (wmask, tmask)

highestSetBit :: Word64 -> Int
highestSetBit w = 63 - countLeadingZeros w

ones :: Int -> Word64
ones n = foldr (.|.) 0 (bit <$> [0..n-1])

replicateBits :: Int -> Int -> Word64 -> Word64
replicateBits total toReplicate val =
    let offsets = (* toReplicate) <$> [0..(total `div` toReplicate) - 1]
    in foldr (\offset old -> old .|. (val `shiftL` offset)) 0 offsets

-- Rotate right with respect to a specific number of bits (rather than
-- using the word size as the number of bits).
rotateRight :: Int
            -- ^ Total bits in the value to be rotated
            -> Int
            -- ^ Amount of right rotation
            -> Word64
            -- ^ Value to rotate
            -> Word64
rotateRight nBits amt val =
    (val `shiftR` amt) .|.
    ((val .&. (ones amt)) `shiftL` (nBits - amt))

instance A.Arbitrary LogicalImm32 where
  arbitrary g = LogicalImm32 <$> A.arbitrary g <*> A.arbitrary g

logicalImm32ImmsField :: Field 6
logicalImm32ImmsField = field 0

logicalImm32ImmrField :: Field 6
logicalImm32ImmrField = field 6

logicalImm32ToBits :: LogicalImm32 -> Word32
logicalImm32ToBits val =
  insert logicalImm32ImmsField (logicalImm32Imms val) $
  insert logicalImm32ImmrField (logicalImm32Immr val) 0

mkLogicalImm32 :: Word32 -> LogicalImm32
mkLogicalImm32 w =
  LogicalImm32 (extract logicalImm32ImmsField w)
               (extract logicalImm32ImmrField w)

logicalImm32Operand :: OperandPayload
logicalImm32Operand =
  OperandPayload { opTypeT = [t| LogicalImm32 |]
                 , opConE  = Just (varE 'mkLogicalImm32)
                 , opWordE = Just (varE 'logicalImm32ToBits)
                 }

data LogicalImm64 = LogicalImm64 { logicalImm64Imms :: W.W 6
                                 , logicalImm64Immr :: W.W 6
                                 , logicalImm64ImmN :: W.W 1
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty LogicalImm64 where
  pPrint (LogicalImm64 s r n) =
      let (imm, _) = decodeBitMasks 64 (fromIntegral $ W.unW n)
                                       (fromIntegral $ W.unW s)
                                       (fromIntegral $ W.unW r) True
      in PP.text $ "#0x" <> showHex imm ""

instance A.Arbitrary LogicalImm64 where
  arbitrary g = LogicalImm64 <$> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g

logicalImm64ImmsField :: Field 6
logicalImm64ImmsField = field 0

logicalImm64ImmrField :: Field 6
logicalImm64ImmrField = field 6

logicalImm64ImmNField :: Field  1
logicalImm64ImmNField = field 1

logicalImm64ToBits :: LogicalImm64 -> Word32
logicalImm64ToBits val =
  insert logicalImm64ImmsField (logicalImm64Imms val) $
  insert logicalImm64ImmNField (logicalImm64ImmN val) $
  insert logicalImm64ImmrField (logicalImm64Immr val) 0

mkLogicalImm64 :: Word32 -> LogicalImm64
mkLogicalImm64 w =
  LogicalImm64 (extract logicalImm64ImmsField w)
               (extract logicalImm64ImmrField w)
               (extract logicalImm64ImmNField w)

logicalImm64Operand :: OperandPayload
logicalImm64Operand =
  OperandPayload { opTypeT = [t| LogicalImm64 |]
                 , opConE  = Just (varE 'mkLogicalImm64)
                 , opWordE = Just (varE 'logicalImm64ToBits)
                 }

data Movimm32Imm = Movimm32Imm { movimm32ImmImm :: W.W 16
                               } deriving (Eq, Ord, Show)

instance PP.Pretty Movimm32Imm where
  pPrint (Movimm32Imm i) = PP.text $ "#0x" <> (showHex (W.unW i) "")

instance A.Arbitrary Movimm32Imm where
  arbitrary g = Movimm32Imm <$> A.arbitrary g

movimm32ImmImmField :: Field 16
movimm32ImmImmField = field 0

movimm32ImmToBits :: Movimm32Imm -> Word32
movimm32ImmToBits val =
  insert movimm32ImmImmField (movimm32ImmImm val) 0

mkMovimm32Imm :: Word32 -> Movimm32Imm
mkMovimm32Imm w =
  Movimm32Imm (extract movimm32ImmImmField w)

movimm32ImmOperand :: OperandPayload
movimm32ImmOperand =
  OperandPayload { opTypeT = [t| Movimm32Imm |]
                 , opConE  = Just (varE 'mkMovimm32Imm)
                 , opWordE = Just (varE 'movimm32ImmToBits)
                 }

data Movimm32Shift = Movimm32Shift { movimm32ShiftShift :: W.W 2
                                   } deriving (Eq, Ord, Show)

instance PP.Pretty Movimm32Shift where
  pPrint (Movimm32Shift s) =
      if s == 0
      then mempty
      else PP.text $ ", lsl #" <> show (s * 16)

instance A.Arbitrary Movimm32Shift where
  arbitrary g = Movimm32Shift <$> A.arbitrary g

movimm32ShiftShiftField :: Field 2
movimm32ShiftShiftField = field 4

movimm32ShiftToBits :: Movimm32Shift -> Word32
movimm32ShiftToBits val =
  insert movimm32ShiftShiftField (movimm32ShiftShift val) 0

mkMovimm32Shift :: Word32 -> Movimm32Shift
mkMovimm32Shift w =
  Movimm32Shift (extract movimm32ShiftShiftField w)

movimm32ShiftOperand :: OperandPayload
movimm32ShiftOperand =
  OperandPayload { opTypeT = [t| Movimm32Shift |]
                 , opConE  = Just (varE 'mkMovimm32Shift)
                 , opWordE = Just (varE 'movimm32ShiftToBits)
                 }

data Movimm64Shift = Movimm64Shift { movimm64ShiftShift :: W.W 2
                                   } deriving (Eq, Ord, Show)

instance PP.Pretty Movimm64Shift where
  pPrint (Movimm64Shift s) =
      if s == 0
      then mempty
      else PP.text $ ", lsl #" <> show (s * 16)

instance A.Arbitrary Movimm64Shift where
  arbitrary g = Movimm64Shift <$> A.arbitrary g

movimm64ShiftShiftField :: Field 2
movimm64ShiftShiftField = field 4

movimm64ShiftToBits :: Movimm64Shift -> Word32
movimm64ShiftToBits val =
  insert movimm64ShiftShiftField (movimm64ShiftShift val) 0

mkMovimm64Shift :: Word32 -> Movimm64Shift
mkMovimm64Shift w =
  Movimm64Shift (extract movimm64ShiftShiftField w)

movimm64ShiftOperand :: OperandPayload
movimm64ShiftOperand =
  OperandPayload { opTypeT = [t| Movimm64Shift |]
                 , opConE  = Just (varE 'mkMovimm64Shift)
                 , opWordE = Just (varE 'movimm64ShiftToBits)
                 }

data MrsSysregOp = MrsSysregOp { mrsSysregOpOp2 :: W.W 3
                               , mrsSysregOpCrm :: W.W 4
                               , mrsSysregOpCrn :: W.W 4
                               , mrsSysregOpOp1 :: W.W 3
                               , mrsSysregOpO0 :: W.W 1
                               , mrsSysregOpHibit :: W.W 1
                               } deriving (Eq, Ord, Show)

instance PP.Pretty MrsSysregOp where
  pPrint _ = PP.text "MrsSysregOp: not implemented"

instance A.Arbitrary MrsSysregOp where
  arbitrary g = MrsSysregOp <$> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g

mrsSysregOpOp2Field :: Field 3
mrsSysregOpOp2Field = field 0

mrsSysregOpCrmField :: Field 4
mrsSysregOpCrmField = field 3

mrsSysregOpCrnField :: Field 4
mrsSysregOpCrnField = field 7

mrsSysregOpOp1Field :: Field 3
mrsSysregOpOp1Field = field 11

mrsSysregOpO0Field :: Field 1
mrsSysregOpO0Field = field 14

mrsSysregOpHibitField :: Field 1
mrsSysregOpHibitField = field 15

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
  MrsSysregOp (extract mrsSysregOpOp2Field w)
              (extract mrsSysregOpCrmField w)
              (extract mrsSysregOpCrnField w)
              (extract mrsSysregOpOp1Field w)
              (extract mrsSysregOpO0Field w)
              (extract mrsSysregOpHibitField w)

mrsSysregOpOperand :: OperandPayload
mrsSysregOpOperand =
  OperandPayload { opTypeT = [t| MrsSysregOp |]
                 , opConE  = Just (varE 'mkMrsSysregOp)
                 , opWordE = Just (varE 'mrsSysregOpToBits)
                 }

data MsrSysregOp = MsrSysregOp { msrSysregOpOp2 :: W.W 3
                               , msrSysregOpCrm :: W.W 4
                               , msrSysregOpCrn :: W.W 4
                               , msrSysregOpOp1 :: W.W 3
                               , msrSysregOpO0 :: W.W 1
                               , msrSysregOpHibit :: W.W 1
                               } deriving (Eq, Ord, Show)

instance PP.Pretty MsrSysregOp where
  pPrint _ = PP.text "MsrSysregOp: not implemented"

instance A.Arbitrary MsrSysregOp where
  arbitrary g = MsrSysregOp <$> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g <*> A.arbitrary g

msrSysregOpOp2Field :: Field 3
msrSysregOpOp2Field = field 0

msrSysregOpCrmField :: Field 4
msrSysregOpCrmField = field 3

msrSysregOpCrnField :: Field 4
msrSysregOpCrnField = field 7

msrSysregOpOp1Field :: Field 3
msrSysregOpOp1Field = field 11

msrSysregOpO0Field :: Field 1
msrSysregOpO0Field = field 14

msrSysregOpHibitField :: Field 1
msrSysregOpHibitField = field 15

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
  MsrSysregOp (extract msrSysregOpOp2Field w)
              (extract msrSysregOpCrmField w)
              (extract msrSysregOpCrnField w)
              (extract msrSysregOpOp1Field w)
              (extract msrSysregOpO0Field w)
              (extract msrSysregOpHibitField w)

msrSysregOpOperand :: OperandPayload
msrSysregOpOperand =
  OperandPayload { opTypeT = [t| MsrSysregOp |]
                 , opConE  = Just (varE 'mkMsrSysregOp)
                 , opWordE = Just (varE 'msrSysregOpToBits)
                 }

data Prfop = Prfop { prfopType :: W.W 5
                   } deriving (Eq, Ord, Show)

instance PP.Pretty Prfop where
  -- See C5.6.144 for prfop values
  pPrint _ = PP.text "Prfop: not implemented"

instance A.Arbitrary Prfop where
  arbitrary g = Prfop <$> A.arbitrary g

prfopTypeField :: Field 5
prfopTypeField = field 0

prfopToBits :: Prfop -> Word32
prfopToBits val =
  insert prfopTypeField (prfopType val) 0

mkPrfop :: Word32 -> Prfop
mkPrfop w =
  Prfop (extract prfopTypeField w)

prfopOperand :: OperandPayload
prfopOperand =
  OperandPayload { opTypeT = [t| Prfop |]
                 , opConE  = Just (varE 'mkPrfop)
                 , opWordE = Just (varE 'prfopToBits)
                 }

data Pstatefield1Op = Pstatefield1Op { pstatefield1OpOp1 :: W.W 3
                                     , pstatefield1OpOp2 :: W.W 3
                                     } deriving (Eq, Ord, Show)

instance PP.Pretty Pstatefield1Op where
  -- See C5.6.130
  pPrint _ = PP.text "Pstatefield1Op: not implemented"

instance A.Arbitrary Pstatefield1Op where
  arbitrary g = Pstatefield1Op <$> A.arbitrary g <*> A.arbitrary g

pstatefield1OpOp1Field :: Field 3
pstatefield1OpOp1Field = field 3

pstatefield1OpOp2Field :: Field 3
pstatefield1OpOp2Field = field 0

pstatefield1OpToBits :: Pstatefield1Op -> Word32
pstatefield1OpToBits val =
  insert pstatefield1OpOp1Field (pstatefield1OpOp1 val) $
  insert pstatefield1OpOp2Field (pstatefield1OpOp2 val) 0

mkPstatefield1Op :: Word32 -> Pstatefield1Op
mkPstatefield1Op w =
  Pstatefield1Op (extract pstatefield1OpOp1Field w)
                 (extract pstatefield1OpOp2Field w)

pstatefield1OpOperand :: OperandPayload
pstatefield1OpOperand =
  OperandPayload { opTypeT = [t| Pstatefield1Op |]
                 , opConE  = Just (varE 'mkPstatefield1Op)
                 , opWordE = Just (varE 'pstatefield1OpToBits)
                 }

data Pstatefield4Op = Pstatefield4Op { pstatefield4OpOp1 :: W.W 3
                                     , pstatefield4OpOp2 :: W.W 3
                                     } deriving (Eq, Ord, Show)

instance PP.Pretty Pstatefield4Op where
  pPrint _ = PP.text "Pstatefield4Op: not implemented"

instance A.Arbitrary Pstatefield4Op where
  arbitrary g = Pstatefield4Op <$> A.arbitrary g <*> A.arbitrary g

pstatefield4OpOp1Field :: Field 3
pstatefield4OpOp1Field = field 3

pstatefield4OpOp2Field :: Field 3
pstatefield4OpOp2Field = field 0

pstatefield4OpToBits :: Pstatefield4Op -> Word32
pstatefield4OpToBits val =
  insert pstatefield4OpOp1Field (pstatefield4OpOp1 val) $
  insert pstatefield4OpOp2Field (pstatefield4OpOp2 val) 0

mkPstatefield4Op :: Word32 -> Pstatefield4Op
mkPstatefield4Op w =
  Pstatefield4Op (extract pstatefield4OpOp1Field w)
                 (extract pstatefield4OpOp2Field w)

pstatefield4OpOperand :: OperandPayload
pstatefield4OpOperand =
  OperandPayload { opTypeT = [t| Pstatefield4Op |]
                 , opConE  = Just (varE 'mkPstatefield4Op)
                 , opWordE = Just (varE 'pstatefield4OpToBits)
                 }

data RoWextend128 = RoWextend128 { roWextend128Sbit :: W.W 1
                                 , roWextend128OptionHiBit :: W.W 1
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty RoWextend128 where
  -- See C6.3.168
  pPrint _ = PP.text "RoWextend128: not implemented"

instance A.Arbitrary RoWextend128 where
  arbitrary g = RoWextend128 <$> A.arbitrary g <*> A.arbitrary g

roWextend128SbitField :: Field 1
roWextend128SbitField = field 0

roWextend128OptionHiBitField :: Field 1
roWextend128OptionHiBitField = field 1

roWextend128ToBits :: RoWextend128 -> Word32
roWextend128ToBits val =
  insert roWextend128SbitField (roWextend128Sbit val) $
  insert roWextend128OptionHiBitField (roWextend128OptionHiBit val) 0

mkRoWextend128 :: Word32 -> RoWextend128
mkRoWextend128 w =
  RoWextend128 (extract roWextend128SbitField w)
               (extract roWextend128OptionHiBitField w)

roWextend128Operand :: OperandPayload
roWextend128Operand =
  OperandPayload { opTypeT = [t| RoWextend128 |]
                 , opConE  = Just (varE 'mkRoWextend128)
                 , opWordE = Just (varE 'roWextend128ToBits)
                 }

data RoWextend16 = RoWextend16 { roWextend16Sbit :: W.W 1
                               , roWextend16OptionHiBit :: W.W 1
                               } deriving (Eq, Ord, Show)

instance PP.Pretty RoWextend16 where
  -- See C5.6.93
  pPrint _ = PP.text "RoWextend16: not implemented"

instance A.Arbitrary RoWextend16 where
  arbitrary g = RoWextend16 <$> A.arbitrary g <*> A.arbitrary g

roWextend16SbitField :: Field 1
roWextend16SbitField = field 0

roWextend16OptionHiBitField :: Field 1
roWextend16OptionHiBitField = field 1

roWextend16ToBits :: RoWextend16 -> Word32
roWextend16ToBits val =
  insert roWextend16SbitField (roWextend16Sbit val) $
  insert roWextend16OptionHiBitField (roWextend16OptionHiBit val) 0

mkRoWextend16 :: Word32 -> RoWextend16
mkRoWextend16 w =
  RoWextend16 (extract roWextend16SbitField w)
              (extract roWextend16OptionHiBitField w)

roWextend16Operand :: OperandPayload
roWextend16Operand =
  OperandPayload { opTypeT = [t| RoWextend16 |]
                 , opConE  = Just (varE 'mkRoWextend16)
                 , opWordE = Just (varE 'roWextend16ToBits)
                 }

data RoWextend32 = RoWextend32 { roWextend32Sbit :: W.W 1
                               , roWextend32OptionHiBit :: W.W 1
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

roWextend32SbitField :: Field 1
roWextend32SbitField = field 0

roWextend32OptionHiBitField :: Field 1
roWextend32OptionHiBitField = field 1

roWextend32ToBits :: RoWextend32 -> Word32
roWextend32ToBits val =
  insert roWextend32SbitField (roWextend32Sbit val) $
  insert roWextend32OptionHiBitField (roWextend32OptionHiBit val) 0

mkRoWextend32 :: Word32 -> RoWextend32
mkRoWextend32 w =
  RoWextend32 (extract roWextend32SbitField w)
              (extract roWextend32OptionHiBitField w)

roWextend32Operand :: OperandPayload
roWextend32Operand =
  OperandPayload { opTypeT = [t| RoWextend32 |]
                 , opConE  = Just (varE 'mkRoWextend32)
                 , opWordE = Just (varE 'roWextend32ToBits)
                 }

data RoWextend64 = RoWextend64 { roWextend64Sbit :: W.W 1
                               , roWextend64OptionHiBit :: W.W 1
                               } deriving (Eq, Ord, Show)

instance PP.Pretty RoWextend64 where
  pPrint _ = PP.text "RoWextend64: not implemented"

instance A.Arbitrary RoWextend64 where
  arbitrary g = RoWextend64 <$> A.arbitrary g <*> A.arbitrary g

roWextend64SbitField :: Field 1
roWextend64SbitField = field 0

roWextend64OptionHiBitField :: Field 1
roWextend64OptionHiBitField = field 1

roWextend64ToBits :: RoWextend64 -> Word32
roWextend64ToBits val =
  insert roWextend64SbitField (roWextend64Sbit val) $
  insert roWextend64OptionHiBitField (roWextend64OptionHiBit val) 0

mkRoWextend64 :: Word32 -> RoWextend64
mkRoWextend64 w =
  RoWextend64 (extract roWextend64SbitField w)
              (extract roWextend64OptionHiBitField w)

roWextend64Operand :: OperandPayload
roWextend64Operand =
  OperandPayload { opTypeT = [t| RoWextend64 |]
                 , opConE  = Just (varE 'mkRoWextend64)
                 , opWordE = Just (varE 'roWextend64ToBits)
                 }

data RoWextend8 = RoWextend8 { roWextend8Sbit :: W.W 1
                             , roWextend8OptionHiBit :: W.W 1
                             } deriving (Eq, Ord, Show)

instance PP.Pretty RoWextend8 where
  pPrint _ = PP.text "RoWextend8: not implemented"

instance A.Arbitrary RoWextend8 where
  arbitrary g = RoWextend8 <$> A.arbitrary g <*> A.arbitrary g

roWextend8SbitField :: Field 1
roWextend8SbitField = field 0

roWextend8OptionHiBitField :: Field 1
roWextend8OptionHiBitField = field 1

roWextend8ToBits :: RoWextend8 -> Word32
roWextend8ToBits val =
  insert roWextend8SbitField (roWextend8Sbit val) $
  insert roWextend8OptionHiBitField (roWextend8OptionHiBit val) 0

mkRoWextend8 :: Word32 -> RoWextend8
mkRoWextend8 w =
  RoWextend8 (extract roWextend8SbitField w)
             (extract roWextend8OptionHiBitField w)

roWextend8Operand :: OperandPayload
roWextend8Operand =
  OperandPayload { opTypeT = [t| RoWextend8 |]
                 , opConE  = Just (varE 'mkRoWextend8)
                 , opWordE = Just (varE 'roWextend8ToBits)
                 }

data RoXextend128 = RoXextend128 { roXextend128Sbit :: W.W 1
                                 , roXextend128OptionHiBit :: W.W 1
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty RoXextend128 where
  -- See C6.3.168
  pPrint _ = PP.text "RoXextend128: not implemented"

instance A.Arbitrary RoXextend128 where
  arbitrary g = RoXextend128 <$> A.arbitrary g <*> A.arbitrary g

roXextend128SbitField :: Field 1
roXextend128SbitField = field 0

roXextend128OptionHiBitField :: Field 1
roXextend128OptionHiBitField = field 1

roXextend128ToBits :: RoXextend128 -> Word32
roXextend128ToBits val =
  insert roXextend128SbitField (roXextend128Sbit val) $
  insert roXextend128OptionHiBitField (roXextend128OptionHiBit val) 0

mkRoXextend128 :: Word32 -> RoXextend128
mkRoXextend128 w =
  RoXextend128 (extract roXextend128SbitField w)
               (extract roXextend128OptionHiBitField w)

roXextend128Operand :: OperandPayload
roXextend128Operand =
  OperandPayload { opTypeT = [t| RoXextend128 |]
                 , opConE  = Just (varE 'mkRoXextend128)
                 , opWordE = Just (varE 'roXextend128ToBits)
                 }

data RoXextend16 = RoXextend16 { roXextend16Sbit :: W.W 1
                               , roXextend16OptionHiBit :: W.W 1
                               } deriving (Eq, Ord, Show)

instance PP.Pretty RoXextend16 where
  -- See C5.6.93
  pPrint _ = PP.text "RoXextend16: not implemented"

instance A.Arbitrary RoXextend16 where
  arbitrary g = RoXextend16 <$> A.arbitrary g <*> A.arbitrary g

roXextend16SbitField :: Field 1
roXextend16SbitField = field 0

roXextend16OptionHiBitField :: Field 1
roXextend16OptionHiBitField = field 1

roXextend16ToBits :: RoXextend16 -> Word32
roXextend16ToBits val =
  insert roXextend16SbitField (roXextend16Sbit val) $
  insert roXextend16OptionHiBitField (roXextend16OptionHiBit val) 0

mkRoXextend16 :: Word32 -> RoXextend16
mkRoXextend16 w =
  RoXextend16 (extract roXextend16SbitField w)
              (extract roXextend16OptionHiBitField w)

roXextend16Operand :: OperandPayload
roXextend16Operand =
  OperandPayload { opTypeT = [t| RoXextend16 |]
                 , opConE  = Just (varE 'mkRoXextend16)
                 , opWordE = Just (varE 'roXextend16ToBits)
                 }

data RoXextend32 = RoXextend32 { roXextend32Sbit :: W.W 1
                               , roXextend32OptionHiBit :: W.W 1
                               } deriving (Eq, Ord, Show)

instance PP.Pretty RoXextend32 where
  pPrint (RoXextend32 s o) =
      let ty = case o of
            0b0 -> "lsl"
            0b1 -> "sxtx"
            _   -> error $ "Invalid RoXextend32 value: " <> show o
          amt = if s == 1 then 2 else 0
      in if amt == 0
         then mempty
         else PP.text ty PP.<+> PP.text (show amt)

instance A.Arbitrary RoXextend32 where
  arbitrary g = RoXextend32 <$> A.arbitrary g <*> A.arbitrary g

roXextend32SbitField :: Field 1
roXextend32SbitField = field 0

roXextend32OptionHiBitField :: Field 1
roXextend32OptionHiBitField = field 1

roXextend32ToBits :: RoXextend32 -> Word32
roXextend32ToBits val =
  insert roXextend32SbitField (roXextend32Sbit val) $
  insert roXextend32OptionHiBitField (roXextend32OptionHiBit val) 0

mkRoXextend32 :: Word32 -> RoXextend32
mkRoXextend32 w =
  RoXextend32 (extract roXextend32SbitField w)
              (extract roXextend32OptionHiBitField w)

roXextend32Operand :: OperandPayload
roXextend32Operand =
  OperandPayload { opTypeT = [t| RoXextend32 |]
                 , opConE  = Just (varE 'mkRoXextend32)
                 , opWordE = Just (varE 'roXextend32ToBits)
                 }

data RoXextend64 = RoXextend64 { roXextend64Sbit :: W.W 1
                               , roXextend64OptionHiBit :: W.W 1
                               } deriving (Eq, Ord, Show)

instance PP.Pretty RoXextend64 where
  pPrint (RoXextend64 s o) =
      let ty = case o of
            0b0 -> "lsl"
            0b1 -> "sxtx"
            _   -> error $ "Invalid RoXextend64 value: " <> show o
          amt = if s == 1 then 3 else 0
      in if amt == 0
         then mempty
         else PP.text ty PP.<+> PP.text (show amt)

instance A.Arbitrary RoXextend64 where
  arbitrary g = RoXextend64 <$> A.arbitrary g <*> A.arbitrary g

roXextend64SbitField :: Field 1
roXextend64SbitField = field 0

roXextend64OptionHiBitField :: Field 1
roXextend64OptionHiBitField = field 1

roXextend64ToBits :: RoXextend64 -> Word32
roXextend64ToBits val =
  insert roXextend64SbitField (roXextend64Sbit val) $
  insert roXextend64OptionHiBitField (roXextend64OptionHiBit val) 0

mkRoXextend64 :: Word32 -> RoXextend64
mkRoXextend64 w =
  RoXextend64 (extract roXextend64SbitField w)
              (extract roXextend64OptionHiBitField w)

roXextend64Operand :: OperandPayload
roXextend64Operand =
  OperandPayload { opTypeT = [t| RoXextend64 |]
                 , opConE  = Just (varE 'mkRoXextend64)
                 , opWordE = Just (varE 'roXextend64ToBits)
                 }

data RoXextend8 = RoXextend8 { roXextend8Sbit :: W.W 1
                             , roXextend8OptionHiBit :: W.W 1
                             } deriving (Eq, Ord, Show)

instance PP.Pretty RoXextend8 where
  pPrint _ = PP.text "RoXextend8: not implemented"

instance A.Arbitrary RoXextend8 where
  arbitrary g = RoXextend8 <$> A.arbitrary g <*> A.arbitrary g

roXextend8SbitField :: Field 1
roXextend8SbitField = field 0

roXextend8OptionHiBitField :: Field 1
roXextend8OptionHiBitField = field 1

roXextend8ToBits :: RoXextend8 -> Word32
roXextend8ToBits val =
  insert roXextend8SbitField (roXextend8Sbit val) $
  insert roXextend8OptionHiBitField (roXextend8OptionHiBit val) 0

mkRoXextend8 :: Word32 -> RoXextend8
mkRoXextend8 w =
  RoXextend8 (extract roXextend8SbitField w)
             (extract roXextend8OptionHiBitField w)

roXextend8Operand :: OperandPayload
roXextend8Operand =
  OperandPayload { opTypeT = [t| RoXextend8 |]
                 , opConE  = Just (varE 'mkRoXextend8)
                 , opWordE = Just (varE 'roXextend8ToBits)
                 }

data Simm7s16 = Simm7s16 { simm7s16Imm :: W.W 7
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Simm7s16 where
  -- C5.6.80
  pPrint (Simm7s16 imm) =
      let signBit = 0b1000000
          signBitExtension = 0xffc0
          v' :: Word16
          v' = if imm .&. signBit == signBit
               then fromIntegral (W.unW imm) .|. signBitExtension
               else fromIntegral (W.unW imm)
          -- Manual sign extension from a 7-bit value to 16-bit.
          v :: Int16
          v = fromIntegral v'
      in PP.char '#' <> (PP.text $ show $ v `shiftL` 4)

instance A.Arbitrary Simm7s16 where
  arbitrary g = Simm7s16 <$> A.arbitrary g

simm7s16ImmField :: Field 7
simm7s16ImmField = field 0

simm7s16ToBits :: Simm7s16 -> Word32
simm7s16ToBits val =
  insert simm7s16ImmField (simm7s16Imm val) 0

mkSimm7s16 :: Word32 -> Simm7s16
mkSimm7s16 w =
  Simm7s16 (extract simm7s16ImmField w)

simm7s16Operand :: OperandPayload
simm7s16Operand =
  OperandPayload { opTypeT = [t| Simm7s16 |]
                 , opConE  = Just (varE 'mkSimm7s16)
                 , opWordE = Just (varE 'simm7s16ToBits)
                 }

data Simm7s4 = Simm7s4 { simm7s4Imm :: W.W 7
                       } deriving (Eq, Ord, Show)

instance PP.Pretty Simm7s4 where
  pPrint (Simm7s4 imm) =
      let signBit = 0b1000000
          -- Manual sign extension from a 7-bit value to 8-bit.
          imm' = fromIntegral $ W.unW imm
          v :: Int
          v = fromIntegral ((imm' .|. ((imm' .&. signBit) `shiftL` 1)) :: Int8)
      in PP.char '#' <> (PP.text $ show $ v `shiftL` 2)

instance A.Arbitrary Simm7s4 where
  arbitrary g = Simm7s4 <$> A.arbitrary g

simm7s4ImmField :: Field 7
simm7s4ImmField = field 0

simm7s4ToBits :: Simm7s4 -> Word32
simm7s4ToBits val =
  insert simm7s4ImmField (simm7s4Imm val) 0

mkSimm7s4 :: Word32 -> Simm7s4
mkSimm7s4 w =
  Simm7s4 (extract simm7s4ImmField w)

simm7s4Operand :: OperandPayload
simm7s4Operand =
  OperandPayload { opTypeT = [t| Simm7s4 |]
                 , opConE  = Just (varE 'mkSimm7s4)
                 , opWordE = Just (varE 'simm7s4ToBits)
                 }

data Simm7s8 = Simm7s8 { simm7s8Imm :: W.W 7
                       } deriving (Eq, Ord, Show)

instance PP.Pretty Simm7s8 where
  pPrint (Simm7s8 imm) =
      let signBit = 0b1000000
          -- Manual sign extension from a 7-bit value to 8-bit.
          imm' = fromIntegral $ W.unW imm
          v :: Int
          v = fromIntegral ((imm' .|. ((imm' .&. signBit) `shiftL` 1)) :: Int8)
      in PP.char '#' <> (PP.text $ show $ v `shiftL` 3)

instance A.Arbitrary Simm7s8 where
  arbitrary g = Simm7s8 <$> A.arbitrary g

simm7s8ImmField :: Field 7
simm7s8ImmField = field 0

simm7s8ToBits :: Simm7s8 -> Word32
simm7s8ToBits val =
  insert simm7s8ImmField (simm7s8Imm val) 0

mkSimm7s8 :: Word32 -> Simm7s8
mkSimm7s8 w =
  Simm7s8 (extract simm7s8ImmField w)

simm7s8Operand :: OperandPayload
simm7s8Operand =
  OperandPayload { opTypeT = [t| Simm7s8 |]
                 , opConE  = Just (varE 'mkSimm7s8)
                 , opWordE = Just (varE 'simm7s8ToBits)
                 }

data Simm9 = Simm9 { simm9Imm :: W.W 9
                   } deriving (Eq, Ord, Show)

instance PP.Pretty Simm9 where
  -- C5.6.86
  pPrint (Simm9 imm) =
      let signBit = 0b100000000
          signBitExtension = 0xff00
          v' :: Word16
          v' = if imm .&. signBit == signBit
               then fromIntegral (W.unW imm) .|. signBitExtension
               else fromIntegral (W.unW imm)
          -- Manual sign extension from a 9-bit value to 16-bit.
          v :: Int16
          v = fromIntegral v'
      in PP.char '#' <> (PP.text $ show v)

instance A.Arbitrary Simm9 where
  arbitrary g = Simm9 <$> A.arbitrary g

simm9ImmField :: Field 9
simm9ImmField = field 0

simm9ToBits :: Simm9 -> Word32
simm9ToBits val =
  insert simm9ImmField (simm9Imm val) 0

mkSimm9 :: Word32 -> Simm9
mkSimm9 w =
  Simm9 (extract simm9ImmField w)

simm9Operand :: OperandPayload
simm9Operand =
  OperandPayload { opTypeT = [t| Simm9 |]
                 , opConE  = Just (varE 'mkSimm9)
                 , opWordE = Just (varE 'simm9ToBits)
                 }

data SysCrOp = SysCrOp { sysCrOpVal :: W.W 4
                       } deriving (Eq, Ord, Show)

instance PP.Pretty SysCrOp where
  pPrint _ = PP.text "SysCrOp: not implemented"

instance A.Arbitrary SysCrOp where
  arbitrary g = SysCrOp <$> A.arbitrary g

sysCrOpValField :: Field 4
sysCrOpValField = field 0

sysCrOpToBits :: SysCrOp -> Word32
sysCrOpToBits val =
  insert sysCrOpValField (sysCrOpVal val) 0

mkSysCrOp :: Word32 -> SysCrOp
mkSysCrOp w =
  SysCrOp (extract sysCrOpValField w)

sysCrOpOperand :: OperandPayload
sysCrOpOperand =
  OperandPayload { opTypeT = [t| SysCrOp |]
                 , opConE  = Just (varE 'mkSysCrOp)
                 , opWordE = Just (varE 'sysCrOpToBits)
                 }

data TbzImm031Diag = TbzImm031Diag { tbzImm031DiagImm :: W.W 5
                                   } deriving (Eq, Ord, Show)

instance PP.Pretty TbzImm031Diag where
  -- See C5.6.206
  pPrint _ = PP.text "TbzImm031Diag: not implemented"

instance A.Arbitrary TbzImm031Diag where
  arbitrary g = TbzImm031Diag <$> A.arbitrary g

tbzImm031DiagImmField :: Field 5
tbzImm031DiagImmField = field 0

tbzImm031DiagToBits :: TbzImm031Diag -> Word32
tbzImm031DiagToBits val =
  insert tbzImm031DiagImmField (tbzImm031DiagImm val) 0

mkTbzImm031Diag :: Word32 -> TbzImm031Diag
mkTbzImm031Diag w =
  TbzImm031Diag (extract tbzImm031DiagImmField w)

tbzImm031DiagOperand :: OperandPayload
tbzImm031DiagOperand =
  OperandPayload { opTypeT = [t| TbzImm031Diag |]
                 , opConE  = Just (varE 'mkTbzImm031Diag)
                 , opWordE = Just (varE 'tbzImm031DiagToBits)
                 }

data TbzImm3263 = TbzImm3263 { tbzImm3263Imm :: W.W 5
                             } deriving (Eq, Ord, Show)

instance PP.Pretty TbzImm3263 where
  -- See C5.6.206
  pPrint _ = PP.text "TbzImm3263: not implemented"

instance A.Arbitrary TbzImm3263 where
  arbitrary g = TbzImm3263 <$> A.arbitrary g

tbzImm3263ImmField :: Field 5
tbzImm3263ImmField = field 0

tbzImm3263ToBits :: TbzImm3263 -> Word32
tbzImm3263ToBits val =
  insert tbzImm3263ImmField (tbzImm3263Imm val) 0

mkTbzImm3263 :: Word32 -> TbzImm3263
mkTbzImm3263 w =
  TbzImm3263 (extract tbzImm3263ImmField w)

tbzImm3263Operand :: OperandPayload
tbzImm3263Operand =
  OperandPayload { opTypeT = [t| TbzImm3263 |]
                 , opConE  = Just (varE 'mkTbzImm3263)
                 , opWordE = Just (varE 'tbzImm3263ToBits)
                 }

data Uimm12s1 = Uimm12s1 { uimm12s1Imm :: W.W 12
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Uimm12s1 where
  pPrint (Uimm12s1 v) =
      PP.char '#' <> (PP.text $ show $ ((fromIntegral $ W.unW v) :: Word32))

instance A.Arbitrary Uimm12s1 where
  arbitrary g = Uimm12s1 <$> A.arbitrary g

uimm12s1ImmField :: Field 12
uimm12s1ImmField = field 0

uimm12s1ToBits :: Uimm12s1 -> Word32
uimm12s1ToBits val =
  insert uimm12s1ImmField (uimm12s1Imm val) 0

mkUimm12s1 :: Word32 -> Uimm12s1
mkUimm12s1 w =
  Uimm12s1 (extract uimm12s1ImmField w)

uimm12s1Operand :: OperandPayload
uimm12s1Operand =
  OperandPayload { opTypeT = [t| Uimm12s1 |]
                 , opConE  = Just (varE 'mkUimm12s1)
                 , opWordE = Just (varE 'uimm12s1ToBits)
                 }

data Uimm12s16 = Uimm12s16 { uimm12s16Imm :: W.W 12
                           } deriving (Eq, Ord, Show)

instance PP.Pretty Uimm12s16 where
  pPrint (Uimm12s16 v) =
    PP.char '#' <> (PP.text $ show $ ((fromIntegral $ W.unW v) :: Word32) `shiftL` 4)

instance A.Arbitrary Uimm12s16 where
  arbitrary g = Uimm12s16 <$> A.arbitrary g

uimm12s16ImmField :: Field 12
uimm12s16ImmField = field 0

uimm12s16ToBits :: Uimm12s16 -> Word32
uimm12s16ToBits val =
  insert uimm12s16ImmField (uimm12s16Imm val) 0

mkUimm12s16 :: Word32 -> Uimm12s16
mkUimm12s16 w =
  Uimm12s16 (extract uimm12s16ImmField w)

uimm12s16Operand :: OperandPayload
uimm12s16Operand =
  OperandPayload { opTypeT = [t| Uimm12s16 |]
                 , opConE  = Just (varE 'mkUimm12s16)
                 , opWordE = Just (varE 'uimm12s16ToBits)
                 }

data Uimm12s2 = Uimm12s2 { uimm12s2Imm :: W.W 12
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Uimm12s2 where
  pPrint (Uimm12s2 v) =
      PP.char '#' <> (PP.text $ show $ ((fromIntegral $ W.unW v) :: Word32) `shiftL` 1)

instance A.Arbitrary Uimm12s2 where
  arbitrary g = Uimm12s2 <$> A.arbitrary g

uimm12s2ImmField :: Field 12
uimm12s2ImmField = field 0

uimm12s2ToBits :: Uimm12s2 -> Word32
uimm12s2ToBits val =
  insert uimm12s2ImmField (uimm12s2Imm val) 0

mkUimm12s2 :: Word32 -> Uimm12s2
mkUimm12s2 w =
  Uimm12s2 (extract uimm12s2ImmField w)

uimm12s2Operand :: OperandPayload
uimm12s2Operand =
  OperandPayload { opTypeT = [t| Uimm12s2 |]
                 , opConE  = Just (varE 'mkUimm12s2)
                 , opWordE = Just (varE 'uimm12s2ToBits)
                 }

data Uimm12s4 = Uimm12s4 { uimm12s4Imm :: W.W 12
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Uimm12s4 where
  pPrint (Uimm12s4 v) =
      PP.char '#' <> (PP.text $ show $ ((fromIntegral $ W.unW v) :: Word32) `shiftL` 2)

instance A.Arbitrary Uimm12s4 where
  arbitrary g = Uimm12s4 <$> A.arbitrary g

uimm12s4ImmField :: Field 12
uimm12s4ImmField = field 0

uimm12s4ToBits :: Uimm12s4 -> Word32
uimm12s4ToBits val =
  insert uimm12s4ImmField (uimm12s4Imm val) 0

mkUimm12s4 :: Word32 -> Uimm12s4
mkUimm12s4 w =
  Uimm12s4 (extract uimm12s4ImmField w)

uimm12s4Operand :: OperandPayload
uimm12s4Operand =
  OperandPayload { opTypeT = [t| Uimm12s4 |]
                 , opConE  = Just (varE 'mkUimm12s4)
                 , opWordE = Just (varE 'uimm12s4ToBits)
                 }

data Uimm12s8 = Uimm12s8 { uimm12s8Imm :: W.W 12
                         } deriving (Eq, Ord, Show)

instance PP.Pretty Uimm12s8 where
  pPrint (Uimm12s8 v) =
      PP.char '#' <> (PP.text $ show $ ((fromIntegral $ W.unW v) :: Word32) `shiftL` 3)

instance A.Arbitrary Uimm12s8 where
  arbitrary g = Uimm12s8 <$> A.arbitrary g

uimm12s8ImmField :: Field 12
uimm12s8ImmField = field 0

uimm12s8ToBits :: Uimm12s8 -> Word32
uimm12s8ToBits val =
  insert uimm12s8ImmField (uimm12s8Imm val) 0

mkUimm12s8 :: Word32 -> Uimm12s8
mkUimm12s8 w =
  Uimm12s8 (extract uimm12s8ImmField w)

uimm12s8Operand :: OperandPayload
uimm12s8Operand =
  OperandPayload { opTypeT = [t| Uimm12s8 |]
                 , opConE  = Just (varE 'mkUimm12s8)
                 , opWordE = Just (varE 'uimm12s8ToBits)
                 }

data Addext = Addext { addextImm :: W.W 3
                     , addextOption :: W.W 3
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
      in PP.text $ ", " <> s <> immS

instance A.Arbitrary Addext where
  arbitrary g = Addext <$> A.arbitrary g <*> A.arbitrary g

addextImmField :: Field 3
addextImmField = field 0

addextOptionField :: Field 3
addextOptionField = field 3

addextToBits :: Addext -> Word32
addextToBits val =
  insert addextImmField (addextImm val) $
  insert addextOptionField (addextOption val) 0

mkAddext :: Word32 -> Addext
mkAddext w =
  Addext (extract addextImmField w)
         (extract addextOptionField w)

addextOperand :: OperandPayload
addextOperand =
  OperandPayload { opTypeT = [t| Addext |]
                 , opConE  = Just (varE 'mkAddext)
                 , opWordE = Just (varE 'addextToBits)
                 }

data FixedpointF32I32 = FixedpointF32I32 { fixedpointF32I32Scale :: W.W 5
                                         } deriving (Eq, Ord, Show)

instance PP.Pretty FixedpointF32I32 where
  pPrint _ = PP.text "FixedpointF32I32: not implemented"

instance A.Arbitrary FixedpointF32I32 where
  arbitrary g = FixedpointF32I32 <$> A.arbitrary g

fixedpointF32I32ScaleField :: Field 5
fixedpointF32I32ScaleField = field 0

fixedpointF32I32ToBits :: FixedpointF32I32 -> Word32
fixedpointF32I32ToBits val =
  insert fixedpointF32I32ScaleField (fixedpointF32I32Scale val) 0

mkFixedpointF32I32 :: Word32 -> FixedpointF32I32
mkFixedpointF32I32 w =
  FixedpointF32I32 (extract fixedpointF32I32ScaleField w)

fixedpointF32I32Operand :: OperandPayload
fixedpointF32I32Operand =
  OperandPayload { opTypeT = [t| FixedpointF32I32 |]
                 , opConE  = Just (varE 'mkFixedpointF32I32)
                 , opWordE = Just (varE 'fixedpointF32I32ToBits)
                 }

data FixedpointF16I32 = FixedpointF16I32 { fixedpointF16I32Scale :: W.W 5
                                         } deriving (Eq, Ord, Show)

instance PP.Pretty FixedpointF16I32 where
  pPrint _ = PP.text "FixedpointF16I32: not implemented"

instance A.Arbitrary FixedpointF16I32 where
  arbitrary g = FixedpointF16I32 <$> A.arbitrary g

fixedpointF16I32ScaleField :: Field 5
fixedpointF16I32ScaleField = field 0

fixedpointF16I32ToBits :: FixedpointF16I32 -> Word32
fixedpointF16I32ToBits val =
  insert fixedpointF16I32ScaleField (fixedpointF16I32Scale val) 0

mkFixedpointF16I32 :: Word32 -> FixedpointF16I32
mkFixedpointF16I32 w =
  FixedpointF16I32 (extract fixedpointF16I32ScaleField w)

fixedpointF16I32Operand :: OperandPayload
fixedpointF16I32Operand =
  OperandPayload { opTypeT = [t| FixedpointF16I32 |]
                 , opConE  = Just (varE 'mkFixedpointF16I32)
                 , opWordE = Just (varE 'fixedpointF16I32ToBits)
                 }

data FixedpointF16I64 = FixedpointF16I64 { fixedpointF16I64Scale :: W.W 6
                                         } deriving (Eq, Ord, Show)

instance PP.Pretty FixedpointF16I64 where
  pPrint _ = PP.text "FixedpointF16I64: not implemented"

instance A.Arbitrary FixedpointF16I64 where
  arbitrary g = FixedpointF16I64 <$> A.arbitrary g

fixedpointF16I64ScaleField :: Field 6
fixedpointF16I64ScaleField = field 0

fixedpointF16I64ToBits :: FixedpointF16I64 -> Word32
fixedpointF16I64ToBits val =
  insert fixedpointF16I64ScaleField (fixedpointF16I64Scale val) 0

mkFixedpointF16I64 :: Word32 -> FixedpointF16I64
mkFixedpointF16I64 w =
  FixedpointF16I64 (extract fixedpointF16I64ScaleField w)

fixedpointF16I64Operand :: OperandPayload
fixedpointF16I64Operand =
  OperandPayload { opTypeT = [t| FixedpointF16I64 |]
                 , opConE  = Just (varE 'mkFixedpointF16I64)
                 , opWordE = Just (varE 'fixedpointF16I64ToBits)
                 }

data FixedpointF32I64 = FixedpointF32I64 { fixedpointF32I64Scale :: W.W 6
                                         } deriving (Eq, Ord, Show)

instance PP.Pretty FixedpointF32I64 where
  pPrint _ = PP.text "FixedpointF32I64: not implemented"

instance A.Arbitrary FixedpointF32I64 where
  arbitrary g = FixedpointF32I64 <$> A.arbitrary g

fixedpointF32I64ScaleField :: Field 6
fixedpointF32I64ScaleField = field 0

fixedpointF32I64ToBits :: FixedpointF32I64 -> Word32
fixedpointF32I64ToBits val =
  insert fixedpointF32I64ScaleField (fixedpointF32I64Scale val) 0

mkFixedpointF32I64 :: Word32 -> FixedpointF32I64
mkFixedpointF32I64 w =
  FixedpointF32I64 (extract fixedpointF32I64ScaleField w)

fixedpointF32I64Operand :: OperandPayload
fixedpointF32I64Operand =
  OperandPayload { opTypeT = [t| FixedpointF32I64 |]
                 , opConE  = Just (varE 'mkFixedpointF32I64)
                 , opWordE = Just (varE 'fixedpointF32I64ToBits)
                 }

data FixedpointF64I32 = FixedpointF64I32 { fixedpointF64I32Scale :: W.W 5
                                         } deriving (Eq, Ord, Show)

instance PP.Pretty FixedpointF64I32 where
  pPrint _ = PP.text "FixedpointF64I32: not implemented"

instance A.Arbitrary FixedpointF64I32 where
  arbitrary g = FixedpointF64I32 <$> A.arbitrary g

fixedpointF64I32ScaleField :: Field 5
fixedpointF64I32ScaleField = field 0

fixedpointF64I32ToBits :: FixedpointF64I32 -> Word32
fixedpointF64I32ToBits val =
  insert fixedpointF64I32ScaleField (fixedpointF64I32Scale val) 0

mkFixedpointF64I32 :: Word32 -> FixedpointF64I32
mkFixedpointF64I32 w =
  FixedpointF64I32 (extract fixedpointF64I32ScaleField w)

fixedpointF64I32Operand :: OperandPayload
fixedpointF64I32Operand =
  OperandPayload { opTypeT = [t| FixedpointF64I32 |]
                 , opConE  = Just (varE 'mkFixedpointF64I32)
                 , opWordE = Just (varE 'fixedpointF64I32ToBits)
                 }

data FixedpointF64I64 = FixedpointF64I64 { fixedpointF64I64Scale :: W.W 6
                                         } deriving (Eq, Ord, Show)

instance PP.Pretty FixedpointF64I64 where
  pPrint _ = PP.text "FixedpointF64I64: not implemented"

instance A.Arbitrary FixedpointF64I64 where
  arbitrary g = FixedpointF64I64 <$> A.arbitrary g

fixedpointF64I64ScaleField :: Field 6
fixedpointF64I64ScaleField = field 0

fixedpointF64I64ToBits :: FixedpointF64I64 -> Word32
fixedpointF64I64ToBits val =
  insert fixedpointF64I64ScaleField (fixedpointF64I64Scale val) 0

mkFixedpointF64I64 :: Word32 -> FixedpointF64I64
mkFixedpointF64I64 w =
  FixedpointF64I64 (extract fixedpointF64I64ScaleField w)

fixedpointF64I64Operand :: OperandPayload
fixedpointF64I64Operand =
  OperandPayload { opTypeT = [t| FixedpointF64I64 |]
                 , opConE  = Just (varE 'mkFixedpointF64I64)
                 , opWordE = Just (varE 'fixedpointF64I64ToBits)
                 }

data Fpimm8 = Fpimm8 { fpimm8Imm :: W.W 8
                     } deriving (Eq, Ord, Show)

instance PP.Pretty Fpimm8 where
  pPrint _ = PP.text "Fpimm8: not implemented"

instance A.Arbitrary Fpimm8 where
  arbitrary g = Fpimm8 <$> A.arbitrary g

fpimm8ImmField :: Field 8
fpimm8ImmField = field 0

fpimm8ToBits :: Fpimm8 -> Word32
fpimm8ToBits val =
  insert fpimm8ImmField (fpimm8Imm val) 0

mkFpimm8 :: Word32 -> Fpimm8
mkFpimm8 w =
  Fpimm8 (extract fpimm8ImmField w)

fpimm8Operand :: OperandPayload
fpimm8Operand =
  OperandPayload { opTypeT = [t| Fpimm8 |]
                 , opConE  = Just (varE 'mkFpimm8)
                 , opWordE = Just (varE 'fpimm8ToBits)
                 }

data Fpimm16 = Fpimm16 { fpimm16Imm :: W.W 8
                       } deriving (Eq, Ord, Show)

instance PP.Pretty Fpimm16 where
  pPrint _ = PP.text "Fpimm16: not implemented"

instance A.Arbitrary Fpimm16 where
  arbitrary g = Fpimm16 <$> A.arbitrary g

fpimm16ImmField :: Field 8
fpimm16ImmField = field 0

fpimm16ToBits :: Fpimm16 -> Word32
fpimm16ToBits val =
  insert fpimm16ImmField (fpimm16Imm val) 0

mkFpimm16 :: Word32 -> Fpimm16
mkFpimm16 w =
  Fpimm16 (extract fpimm16ImmField w)

fpimm16Operand :: OperandPayload
fpimm16Operand =
  OperandPayload { opTypeT = [t| Fpimm16 |]
                 , opConE  = Just (varE 'mkFpimm16)
                 , opWordE = Just (varE 'fpimm16ToBits)
                 }

data Fpimm32 = Fpimm32 { fpimm32Imm :: W.W 8
                       } deriving (Eq, Ord, Show)

instance PP.Pretty Fpimm32 where
  pPrint _ = PP.text "Fpimm32: not implemented"

instance A.Arbitrary Fpimm32 where
  arbitrary g = Fpimm32 <$> A.arbitrary g

fpimm32ImmField :: Field 8
fpimm32ImmField = field 0

fpimm32ToBits :: Fpimm32 -> Word32
fpimm32ToBits val =
  insert fpimm32ImmField (fpimm32Imm val) 0

mkFpimm32 :: Word32 -> Fpimm32
mkFpimm32 w =
  Fpimm32 (extract fpimm32ImmField w)

fpimm32Operand :: OperandPayload
fpimm32Operand =
  OperandPayload { opTypeT = [t| Fpimm32 |]
                 , opConE  = Just (varE 'mkFpimm32)
                 , opWordE = Just (varE 'fpimm32ToBits)
                 }

data Fpimm64 = Fpimm64 { fpimm64Imm :: W.W 8
                       } deriving (Eq, Ord, Show)

instance PP.Pretty Fpimm64 where
  pPrint _ = PP.text "Fpimm64: not implemented"

instance A.Arbitrary Fpimm64 where
  arbitrary g = Fpimm64 <$> A.arbitrary g

fpimm64ImmField :: Field 8
fpimm64ImmField = field 0

fpimm64ToBits :: Fpimm64 -> Word32
fpimm64ToBits val =
  insert fpimm64ImmField (fpimm64Imm val) 0

mkFpimm64 :: Word32 -> Fpimm64
mkFpimm64 w =
  Fpimm64 (extract fpimm64ImmField w)

fpimm64Operand :: OperandPayload
fpimm64Operand =
  OperandPayload { opTypeT = [t| Fpimm64 |]
                 , opConE  = Just (varE 'mkFpimm64)
                 , opWordE = Just (varE 'fpimm64ToBits)
                 }

data Addshift64 = Addshift64 { addshift64Imm :: W.W 6
                             , addshift64Shift :: W.W 2
                             } deriving (Eq, Ord, Show)

instance PP.Pretty Addshift64 where
  pPrint (Addshift64 imm s) =
      if imm == 0
      then mempty
      else let ty = case s of
                      0b0 -> "lsl"
                      0b1 -> "lsr"
                      0b10 -> "asr"
                      _ -> "<reserved>"
           in PP.text $ ", " <> ty <> " " <> show imm

instance A.Arbitrary Addshift64 where
  arbitrary g = Addshift64 <$> A.arbitrary g <*> A.arbitrary g

addshift64ImmField :: Field 6
addshift64ImmField = field 0

addshift64ShiftField :: Field 2
addshift64ShiftField = field 6

addshift64ToBits :: Addshift64 -> Word32
addshift64ToBits val =
  insert addshift64ImmField (addshift64Imm val) $
  insert addshift64ShiftField (addshift64Shift val) 0

mkAddshift64 :: Word32 -> Addshift64
mkAddshift64 w =
  Addshift64 (extract addshift64ImmField w)
             (extract addshift64ShiftField w)

addshift64Operand :: OperandPayload
addshift64Operand =
  OperandPayload { opTypeT = [t| Addshift64 |]
                 , opConE  = Just (varE 'mkAddshift64)
                 , opWordE = Just (varE 'addshift64ToBits)
                 }

data V64 = V64 { v64Reg :: W.W 5
               } deriving (Eq, Ord, Show)

instance PP.Pretty V64 where
  pPrint _ = PP.text "V64: not implemented"

instance A.Arbitrary V64 where
  arbitrary g = V64 <$> A.arbitrary g

v64RegField :: Field 5
v64RegField = field 0

v64ToBits :: V64 -> Word32
v64ToBits val =
  insert v64RegField (v64Reg val) 0

mkV64 :: Word32 -> V64
mkV64 w =
  V64 (extract v64RegField w)

v64Operand :: OperandPayload
v64Operand =
  OperandPayload { opTypeT = [t| V64 |]
                 , opConE  = Just (varE 'mkV64)
                 , opWordE = Just (varE 'v64ToBits)
                 }

data VecListFourb = VecListFourb { vecListFourbReg :: W.W 5
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty VecListFourb where
  pPrint _ = PP.text "VecListFourb: not implemented"

instance A.Arbitrary VecListFourb where
  arbitrary g = VecListFourb <$> A.arbitrary g

vecListFourbRegField :: Field 5
vecListFourbRegField = field 0

vecListFourbToBits :: VecListFourb -> Word32
vecListFourbToBits val =
  insert vecListFourbRegField (vecListFourbReg val) 0

mkVecListFourb :: Word32 -> VecListFourb
mkVecListFourb w =
  VecListFourb (extract vecListFourbRegField w)

vecListFourbOperand :: OperandPayload
vecListFourbOperand =
  OperandPayload { opTypeT = [t| VecListFourb |]
                 , opConE  = Just (varE 'mkVecListFourb)
                 , opWordE = Just (varE 'vecListFourbToBits)
                 }

data VecListFourd = VecListFourd { vecListFourdReg :: W.W 5
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty VecListFourd where
  pPrint _ = PP.text "VecListFourd: not implemented"

instance A.Arbitrary VecListFourd where
  arbitrary g = VecListFourd <$> A.arbitrary g

vecListFourdRegField :: Field 5
vecListFourdRegField = field 0

vecListFourdToBits :: VecListFourd -> Word32
vecListFourdToBits val =
  insert vecListFourdRegField (vecListFourdReg val) 0

mkVecListFourd :: Word32 -> VecListFourd
mkVecListFourd w =
  VecListFourd (extract vecListFourdRegField w)

vecListFourdOperand :: OperandPayload
vecListFourdOperand =
  OperandPayload { opTypeT = [t| VecListFourd |]
                 , opConE  = Just (varE 'mkVecListFourd)
                 , opWordE = Just (varE 'vecListFourdToBits)
                 }

data VecListFourh = VecListFourh { vecListFourhReg :: W.W 5
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty VecListFourh where
  pPrint _ = PP.text "VecListFourh: not implemented"

instance A.Arbitrary VecListFourh where
  arbitrary g = VecListFourh <$> A.arbitrary g

vecListFourhRegField :: Field 5
vecListFourhRegField = field 0

vecListFourhToBits :: VecListFourh -> Word32
vecListFourhToBits val =
  insert vecListFourhRegField (vecListFourhReg val) 0

mkVecListFourh :: Word32 -> VecListFourh
mkVecListFourh w =
  VecListFourh (extract vecListFourhRegField w)

vecListFourhOperand :: OperandPayload
vecListFourhOperand =
  OperandPayload { opTypeT = [t| VecListFourh |]
                 , opConE  = Just (varE 'mkVecListFourh)
                 , opWordE = Just (varE 'vecListFourhToBits)
                 }

data VecListFours = VecListFours { vecListFoursReg :: W.W 5
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty VecListFours where
  pPrint _ = PP.text "VecListFours: not implemented"

instance A.Arbitrary VecListFours where
  arbitrary g = VecListFours <$> A.arbitrary g

vecListFoursRegField :: Field 5
vecListFoursRegField = field 0

vecListFoursToBits :: VecListFours -> Word32
vecListFoursToBits val =
  insert vecListFoursRegField (vecListFoursReg val) 0

mkVecListFours :: Word32 -> VecListFours
mkVecListFours w =
  VecListFours (extract vecListFoursRegField w)

vecListFoursOperand :: OperandPayload
vecListFoursOperand =
  OperandPayload { opTypeT = [t| VecListFours |]
                 , opConE  = Just (varE 'mkVecListFours)
                 , opWordE = Just (varE 'vecListFoursToBits)
                 }

data VecListOneb = VecListOneb { vecListOnebReg :: W.W 5
                               } deriving (Eq, Ord, Show)

instance PP.Pretty VecListOneb where
  pPrint _ = PP.text "VecListOneb: not implemented"

instance A.Arbitrary VecListOneb where
  arbitrary g = VecListOneb <$> A.arbitrary g

vecListOnebRegField :: Field 5
vecListOnebRegField = field 0

vecListOnebToBits :: VecListOneb -> Word32
vecListOnebToBits val =
  insert vecListOnebRegField (vecListOnebReg val) 0

mkVecListOneb :: Word32 -> VecListOneb
mkVecListOneb w =
  VecListOneb (extract vecListOnebRegField w)

vecListOnebOperand :: OperandPayload
vecListOnebOperand =
  OperandPayload { opTypeT = [t| VecListOneb |]
                 , opConE  = Just (varE 'mkVecListOneb)
                 , opWordE = Just (varE 'vecListOnebToBits)
                 }

data VecListOned = VecListOned { vecListOnedReg :: W.W 5
                               } deriving (Eq, Ord, Show)

instance PP.Pretty VecListOned where
  pPrint _ = PP.text "VecListOned: not implemented"

instance A.Arbitrary VecListOned where
  arbitrary g = VecListOned <$> A.arbitrary g

vecListOnedRegField :: Field 5
vecListOnedRegField = field 0

vecListOnedToBits :: VecListOned -> Word32
vecListOnedToBits val =
  insert vecListOnedRegField (vecListOnedReg val) 0

mkVecListOned :: Word32 -> VecListOned
mkVecListOned w =
  VecListOned (extract vecListOnedRegField w)

vecListOnedOperand :: OperandPayload
vecListOnedOperand =
  OperandPayload { opTypeT = [t| VecListOned |]
                 , opConE  = Just (varE 'mkVecListOned)
                 , opWordE = Just (varE 'vecListOnedToBits)
                 }

data VecListOneh = VecListOneh { vecListOnehReg :: W.W 5
                               } deriving (Eq, Ord, Show)

instance PP.Pretty VecListOneh where
  pPrint _ = PP.text "VecListOneh: not implemented"

instance A.Arbitrary VecListOneh where
  arbitrary g = VecListOneh <$> A.arbitrary g

vecListOnehRegField :: Field 5
vecListOnehRegField = field 0

vecListOnehToBits :: VecListOneh -> Word32
vecListOnehToBits val =
  insert vecListOnehRegField (vecListOnehReg val) 0

mkVecListOneh :: Word32 -> VecListOneh
mkVecListOneh w =
  VecListOneh (extract vecListOnehRegField w)

vecListOnehOperand :: OperandPayload
vecListOnehOperand =
  OperandPayload { opTypeT = [t| VecListOneh |]
                 , opConE  = Just (varE 'mkVecListOneh)
                 , opWordE = Just (varE 'vecListOnehToBits)
                 }

data VecListOnes = VecListOnes { vecListOnesReg :: W.W 5
                               } deriving (Eq, Ord, Show)

instance PP.Pretty VecListOnes where
  pPrint _ = PP.text "VecListOnes: not implemented"

instance A.Arbitrary VecListOnes where
  arbitrary g = VecListOnes <$> A.arbitrary g

vecListOnesRegField :: Field 5
vecListOnesRegField = field 0

vecListOnesToBits :: VecListOnes -> Word32
vecListOnesToBits val =
  insert vecListOnesRegField (vecListOnesReg val) 0

mkVecListOnes :: Word32 -> VecListOnes
mkVecListOnes w =
  VecListOnes (extract vecListOnesRegField w)

vecListOnesOperand :: OperandPayload
vecListOnesOperand =
  OperandPayload { opTypeT = [t| VecListOnes |]
                 , opConE  = Just (varE 'mkVecListOnes)
                 , opWordE = Just (varE 'vecListOnesToBits)
                 }

data VecListThreeb = VecListThreeb { vecListThreebReg :: W.W 5
                                   } deriving (Eq, Ord, Show)

instance PP.Pretty VecListThreeb where
  pPrint _ = PP.text "VecListThreeb: not implemented"

instance A.Arbitrary VecListThreeb where
  arbitrary g = VecListThreeb <$> A.arbitrary g

vecListThreebRegField :: Field 5
vecListThreebRegField = field 0

vecListThreebToBits :: VecListThreeb -> Word32
vecListThreebToBits val =
  insert vecListThreebRegField (vecListThreebReg val) 0

mkVecListThreeb :: Word32 -> VecListThreeb
mkVecListThreeb w =
  VecListThreeb (extract vecListThreebRegField w)

vecListThreebOperand :: OperandPayload
vecListThreebOperand =
  OperandPayload { opTypeT = [t| VecListThreeb |]
                 , opConE  = Just (varE 'mkVecListThreeb)
                 , opWordE = Just (varE 'vecListThreebToBits)
                 }

data VecListThreed = VecListThreed { vecListThreedReg :: W.W 5
                                   } deriving (Eq, Ord, Show)

instance PP.Pretty VecListThreed where
  pPrint _ = PP.text "VecListThreed: not implemented"

instance A.Arbitrary VecListThreed where
  arbitrary g = VecListThreed <$> A.arbitrary g

vecListThreedRegField :: Field 5
vecListThreedRegField = field 0

vecListThreedToBits :: VecListThreed -> Word32
vecListThreedToBits val =
  insert vecListThreedRegField (vecListThreedReg val) 0

mkVecListThreed :: Word32 -> VecListThreed
mkVecListThreed w =
  VecListThreed (extract vecListThreedRegField w)

vecListThreedOperand :: OperandPayload
vecListThreedOperand =
  OperandPayload { opTypeT = [t| VecListThreed |]
                 , opConE  = Just (varE 'mkVecListThreed)
                 , opWordE = Just (varE 'vecListThreedToBits)
                 }

data VecListThreeh = VecListThreeh { vecListThreehReg :: W.W 5
                                   } deriving (Eq, Ord, Show)

instance PP.Pretty VecListThreeh where
  pPrint _ = PP.text "VecListThreeh: not implemented"

instance A.Arbitrary VecListThreeh where
  arbitrary g = VecListThreeh <$> A.arbitrary g

vecListThreehRegField :: Field 5
vecListThreehRegField = field 0

vecListThreehToBits :: VecListThreeh -> Word32
vecListThreehToBits val =
  insert vecListThreehRegField (vecListThreehReg val) 0

mkVecListThreeh :: Word32 -> VecListThreeh
mkVecListThreeh w =
  VecListThreeh (extract vecListThreehRegField w)

vecListThreehOperand :: OperandPayload
vecListThreehOperand =
  OperandPayload { opTypeT = [t| VecListThreeh |]
                 , opConE  = Just (varE 'mkVecListThreeh)
                 , opWordE = Just (varE 'vecListThreehToBits)
                 }

data VecListThrees = VecListThrees { vecListThreesReg :: W.W 5
                                   } deriving (Eq, Ord, Show)

instance PP.Pretty VecListThrees where
  pPrint _ = PP.text "VecListThrees: not implemented"

instance A.Arbitrary VecListThrees where
  arbitrary g = VecListThrees <$> A.arbitrary g

vecListThreesRegField :: Field 5
vecListThreesRegField = field 0

vecListThreesToBits :: VecListThrees -> Word32
vecListThreesToBits val =
  insert vecListThreesRegField (vecListThreesReg val) 0

mkVecListThrees :: Word32 -> VecListThrees
mkVecListThrees w =
  VecListThrees (extract vecListThreesRegField w)

vecListThreesOperand :: OperandPayload
vecListThreesOperand =
  OperandPayload { opTypeT = [t| VecListThrees |]
                 , opConE  = Just (varE 'mkVecListThrees)
                 , opWordE = Just (varE 'vecListThreesToBits)
                 }

data VecListTwob = VecListTwob { vecListTwobReg :: W.W 5
                               } deriving (Eq, Ord, Show)

instance PP.Pretty VecListTwob where
  pPrint _ = PP.text "VecListTwob: not implemented"

instance A.Arbitrary VecListTwob where
  arbitrary g = VecListTwob <$> A.arbitrary g

vecListTwobRegField :: Field 5
vecListTwobRegField = field 0

vecListTwobToBits :: VecListTwob -> Word32
vecListTwobToBits val =
  insert vecListTwobRegField (vecListTwobReg val) 0

mkVecListTwob :: Word32 -> VecListTwob
mkVecListTwob w =
  VecListTwob (extract vecListTwobRegField w)

vecListTwobOperand :: OperandPayload
vecListTwobOperand =
  OperandPayload { opTypeT = [t| VecListTwob |]
                 , opConE  = Just (varE 'mkVecListTwob)
                 , opWordE = Just (varE 'vecListTwobToBits)
                 }

data VecListTwod = VecListTwod { vecListTwodReg :: W.W 5
                               } deriving (Eq, Ord, Show)

instance PP.Pretty VecListTwod where
  pPrint _ = PP.text "VecListTwod: not implemented"

instance A.Arbitrary VecListTwod where
  arbitrary g = VecListTwod <$> A.arbitrary g

vecListTwodRegField :: Field 5
vecListTwodRegField = field 0

vecListTwodToBits :: VecListTwod -> Word32
vecListTwodToBits val =
  insert vecListTwodRegField (vecListTwodReg val) 0

mkVecListTwod :: Word32 -> VecListTwod
mkVecListTwod w =
  VecListTwod (extract vecListTwodRegField w)

vecListTwodOperand :: OperandPayload
vecListTwodOperand =
  OperandPayload { opTypeT = [t| VecListTwod |]
                 , opConE  = Just (varE 'mkVecListTwod)
                 , opWordE = Just (varE 'vecListTwodToBits)
                 }

data VecListTwoh = VecListTwoh { vecListTwohReg :: W.W 5
                               } deriving (Eq, Ord, Show)

instance PP.Pretty VecListTwoh where
  pPrint _ = PP.text "VecListTwoh: not implemented"

instance A.Arbitrary VecListTwoh where
  arbitrary g = VecListTwoh <$> A.arbitrary g

vecListTwohRegField :: Field 5
vecListTwohRegField = field 0

vecListTwohToBits :: VecListTwoh -> Word32
vecListTwohToBits val =
  insert vecListTwohRegField (vecListTwohReg val) 0

mkVecListTwoh :: Word32 -> VecListTwoh
mkVecListTwoh w =
  VecListTwoh (extract vecListTwohRegField w)

vecListTwohOperand :: OperandPayload
vecListTwohOperand =
  OperandPayload { opTypeT = [t| VecListTwoh |]
                 , opConE  = Just (varE 'mkVecListTwoh)
                 , opWordE = Just (varE 'vecListTwohToBits)
                 }

data VecListTwos = VecListTwos { vecListTwosReg :: W.W 5
                               } deriving (Eq, Ord, Show)

instance PP.Pretty VecListTwos where
  pPrint _ = PP.text "VecListTwos: not implemented"

instance A.Arbitrary VecListTwos where
  arbitrary g = VecListTwos <$> A.arbitrary g

vecListTwosRegField :: Field 5
vecListTwosRegField = field 0

vecListTwosToBits :: VecListTwos -> Word32
vecListTwosToBits val =
  insert vecListTwosRegField (vecListTwosReg val) 0

mkVecListTwos :: Word32 -> VecListTwos
mkVecListTwos w =
  VecListTwos (extract vecListTwosRegField w)

vecListTwosOperand :: OperandPayload
vecListTwosOperand =
  OperandPayload { opTypeT = [t| VecListTwos |]
                 , opConE  = Just (varE 'mkVecListTwos)
                 , opWordE = Just (varE 'vecListTwosToBits)
                 }

data VectorIndexD = VectorIndexD { vectorIndexDVal :: W.W 1
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty VectorIndexD where
  pPrint _ = PP.text "VectorIndexD: not implemented"

instance A.Arbitrary VectorIndexD where
  arbitrary g = VectorIndexD <$> A.arbitrary g

vectorIndexDValField :: Field 1
vectorIndexDValField = field 0

vectorIndexDToBits :: VectorIndexD -> Word32
vectorIndexDToBits val =
  insert vectorIndexDValField (vectorIndexDVal val) 0

mkVectorIndexD :: Word32 -> VectorIndexD
mkVectorIndexD w =
  VectorIndexD (extract vectorIndexDValField w)

vectorIndexDOperand :: OperandPayload
vectorIndexDOperand =
  OperandPayload { opTypeT = [t| VectorIndexD |]
                 , opConE  = Just (varE 'mkVectorIndexD)
                 , opWordE = Just (varE 'vectorIndexDToBits)
                 }

data VectorIndexB = VectorIndexB { vectorIndexBVal :: W.W 3
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty VectorIndexB where
  pPrint _ = PP.text "VectorIndexB: not implemented"

instance A.Arbitrary VectorIndexB where
  arbitrary g = VectorIndexB <$> A.arbitrary g

vectorIndexBValField :: Field 3
vectorIndexBValField = field 0

vectorIndexBToBits :: VectorIndexB -> Word32
vectorIndexBToBits val =
  insert vectorIndexBValField (vectorIndexBVal val) 0

mkVectorIndexB :: Word32 -> VectorIndexB
mkVectorIndexB w =
  VectorIndexB (extract vectorIndexBValField w)

vectorIndexBOperand :: OperandPayload
vectorIndexBOperand =
  OperandPayload { opTypeT = [t| VectorIndexB |]
                 , opConE  = Just (varE 'mkVectorIndexB)
                 , opWordE = Just (varE 'vectorIndexBToBits)
                 }

data VectorIndexH = VectorIndexH { vectorIndexHVal :: W.W 3
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty VectorIndexH where
  pPrint _ = PP.text "VectorIndexH: not implemented"

instance A.Arbitrary VectorIndexH where
  arbitrary g = VectorIndexH <$> A.arbitrary g

vectorIndexHValField :: Field 3
vectorIndexHValField = field 0

vectorIndexHToBits :: VectorIndexH -> Word32
vectorIndexHToBits val =
  insert vectorIndexHValField (vectorIndexHVal val) 0

mkVectorIndexH :: Word32 -> VectorIndexH
mkVectorIndexH w =
  VectorIndexH (extract vectorIndexHValField w)

vectorIndexHOperand :: OperandPayload
vectorIndexHOperand =
  OperandPayload { opTypeT = [t| VectorIndexH |]
                 , opConE  = Just (varE 'mkVectorIndexH)
                 , opWordE = Just (varE 'vectorIndexHToBits)
                 }

data VectorIndexS = VectorIndexS { vectorIndexSVal :: W.W 2
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty VectorIndexS where
  pPrint _ = PP.text "VectorIndexS: not implemented"

instance A.Arbitrary VectorIndexS where
  arbitrary g = VectorIndexS <$> A.arbitrary g

vectorIndexSValField :: Field 2
vectorIndexSValField = field 0

vectorIndexSToBits :: VectorIndexS -> Word32
vectorIndexSToBits val =
  insert vectorIndexSValField (vectorIndexSVal val) 0

mkVectorIndexS :: Word32 -> VectorIndexS
mkVectorIndexS w =
  VectorIndexS (extract vectorIndexSValField w)

vectorIndexSOperand :: OperandPayload
vectorIndexSOperand =
  OperandPayload { opTypeT = [t| VectorIndexS |]
                 , opConE  = Just (varE 'mkVectorIndexS)
                 , opWordE = Just (varE 'vectorIndexSToBits)
                 }

data Simdimmtype10 = Simdimmtype10 { simdimmtype10Imm :: W.W 8
                                   } deriving (Eq, Ord, Show)

instance PP.Pretty Simdimmtype10 where
  pPrint _ = PP.text "Simdimmtype10: not implemented"

instance A.Arbitrary Simdimmtype10 where
  arbitrary g = Simdimmtype10 <$> A.arbitrary g

simdimmtype10ImmField :: Field 8
simdimmtype10ImmField = field 0

simdimmtype10ToBits :: Simdimmtype10 -> Word32
simdimmtype10ToBits val =
  insert simdimmtype10ImmField (simdimmtype10Imm val) 0

mkSimdimmtype10 :: Word32 -> Simdimmtype10
mkSimdimmtype10 w =
  Simdimmtype10 (extract simdimmtype10ImmField w)

simdimmtype10Operand :: OperandPayload
simdimmtype10Operand =
  OperandPayload { opTypeT = [t| Simdimmtype10 |]
                 , opConE  = Just (varE 'mkSimdimmtype10)
                 , opWordE = Just (varE 'simdimmtype10ToBits)
                 }

data VecshiftL16 = VecshiftL16 { vecshiftL16Imm :: W.W 4
                               } deriving (Eq, Ord, Show)

instance PP.Pretty VecshiftL16 where
  pPrint _ = PP.text "VecshiftL16: not implemented"

instance A.Arbitrary VecshiftL16 where
  arbitrary g = VecshiftL16 <$> A.arbitrary g

vecshiftL16ImmField :: Field 4
vecshiftL16ImmField = field 0

vecshiftL16ToBits :: VecshiftL16 -> Word32
vecshiftL16ToBits val =
  insert vecshiftL16ImmField (vecshiftL16Imm val) 0

mkVecshiftL16 :: Word32 -> VecshiftL16
mkVecshiftL16 w =
  VecshiftL16 (extract vecshiftL16ImmField w)

vecshiftL16Operand :: OperandPayload
vecshiftL16Operand =
  OperandPayload { opTypeT = [t| VecshiftL16 |]
                 , opConE  = Just (varE 'mkVecshiftL16)
                 , opWordE = Just (varE 'vecshiftL16ToBits)
                 }

data VecshiftL8 = VecshiftL8 { vecshiftL8Imm :: W.W 3
                             } deriving (Eq, Ord, Show)

instance PP.Pretty VecshiftL8 where
  pPrint _ = PP.text "VecshiftL8: not implemented"

instance A.Arbitrary VecshiftL8 where
  arbitrary g = VecshiftL8 <$> A.arbitrary g

vecshiftL8ImmField :: Field 3
vecshiftL8ImmField = field 0

vecshiftL8ToBits :: VecshiftL8 -> Word32
vecshiftL8ToBits val =
  insert vecshiftL8ImmField (vecshiftL8Imm val) 0

mkVecshiftL8 :: Word32 -> VecshiftL8
mkVecshiftL8 w =
  VecshiftL8 (extract vecshiftL8ImmField w)

vecshiftL8Operand :: OperandPayload
vecshiftL8Operand =
  OperandPayload { opTypeT = [t| VecshiftL8 |]
                 , opConE  = Just (varE 'mkVecshiftL8)
                 , opWordE = Just (varE 'vecshiftL8ToBits)
                 }

data VecshiftL32 = VecshiftL32 { vecshiftL32Imm :: W.W 5
                               } deriving (Eq, Ord, Show)

instance PP.Pretty VecshiftL32 where
  pPrint _ = PP.text "VecshiftL32: not implemented"

instance A.Arbitrary VecshiftL32 where
  arbitrary g = VecshiftL32 <$> A.arbitrary g

vecshiftL32ImmField :: Field 5
vecshiftL32ImmField = field 0

vecshiftL32ToBits :: VecshiftL32 -> Word32
vecshiftL32ToBits val =
  insert vecshiftL32ImmField (vecshiftL32Imm val) 0

mkVecshiftL32 :: Word32 -> VecshiftL32
mkVecshiftL32 w =
  VecshiftL32 (extract vecshiftL32ImmField w)

vecshiftL32Operand :: OperandPayload
vecshiftL32Operand =
  OperandPayload { opTypeT = [t| VecshiftL32 |]
                 , opConE  = Just (varE 'mkVecshiftL32)
                 , opWordE = Just (varE 'vecshiftL32ToBits)
                 }

data VecshiftL64 = VecshiftL64 { vecshiftL64Imm :: W.W 6
                               } deriving (Eq, Ord, Show)

instance PP.Pretty VecshiftL64 where
  pPrint _ = PP.text "VecshiftL64: not implemented"

instance A.Arbitrary VecshiftL64 where
  arbitrary g = VecshiftL64 <$> A.arbitrary g

vecshiftL64ImmField :: Field 6
vecshiftL64ImmField = field 0

vecshiftL64ToBits :: VecshiftL64 -> Word32
vecshiftL64ToBits val =
  insert vecshiftL64ImmField (vecshiftL64Imm val) 0

mkVecshiftL64 :: Word32 -> VecshiftL64
mkVecshiftL64 w =
  VecshiftL64 (extract vecshiftL64ImmField w)

vecshiftL64Operand :: OperandPayload
vecshiftL64Operand =
  OperandPayload { opTypeT = [t| VecshiftL64 |]
                 , opConE  = Just (varE 'mkVecshiftL64)
                 , opWordE = Just (varE 'vecshiftL64ToBits)
                 }

data VecshiftR16 = VecshiftR16 { vecshiftR16Imm :: W.W 4
                               } deriving (Eq, Ord, Show)

instance PP.Pretty VecshiftR16 where
  pPrint _ = PP.text "VecshiftR16: not implemented"

instance A.Arbitrary VecshiftR16 where
  arbitrary g = VecshiftR16 <$> A.arbitrary g

vecshiftR16ImmField :: Field 4
vecshiftR16ImmField = field 0

vecshiftR16ToBits :: VecshiftR16 -> Word32
vecshiftR16ToBits val =
  insert vecshiftR16ImmField (vecshiftR16Imm val) 0

mkVecshiftR16 :: Word32 -> VecshiftR16
mkVecshiftR16 w =
  VecshiftR16 (extract vecshiftR16ImmField w)

vecshiftR16Operand :: OperandPayload
vecshiftR16Operand =
  OperandPayload { opTypeT = [t| VecshiftR16 |]
                 , opConE  = Just (varE 'mkVecshiftR16)
                 , opWordE = Just (varE 'vecshiftR16ToBits)
                 }

data VecshiftR64 = VecshiftR64 { vecshiftR64Imm :: W.W 6
                               } deriving (Eq, Ord, Show)

instance PP.Pretty VecshiftR64 where
  pPrint (VecshiftR64 imm) =
      let v = 128 - ((0b1 `shiftL` 6) .|. imm)
      in PP.text $ "#" <> show v

instance A.Arbitrary VecshiftR64 where
  arbitrary g = VecshiftR64 <$> A.arbitrary g

vecshiftR64ImmField :: Field 6
vecshiftR64ImmField = field 0

vecshiftR64ToBits :: VecshiftR64 -> Word32
vecshiftR64ToBits val =
  insert vecshiftR64ImmField (vecshiftR64Imm val) 0

mkVecshiftR64 :: Word32 -> VecshiftR64
mkVecshiftR64 w =
  VecshiftR64 (extract vecshiftR64ImmField w)

vecshiftR64Operand :: OperandPayload
vecshiftR64Operand =
  OperandPayload { opTypeT = [t| VecshiftR64 |]
                 , opConE  = Just (varE 'mkVecshiftR64)
                 , opWordE = Just (varE 'vecshiftR64ToBits)
                 }

data VecshiftR32 = VecshiftR32 { vecshiftR32Imm :: W.W 5
                               } deriving (Eq, Ord, Show)

instance PP.Pretty VecshiftR32 where
  pPrint _ = PP.text "VecshiftR32: not implemented"

instance A.Arbitrary VecshiftR32 where
  arbitrary g = VecshiftR32 <$> A.arbitrary g

vecshiftR32ImmField :: Field 5
vecshiftR32ImmField = field 0

vecshiftR32ToBits :: VecshiftR32 -> Word32
vecshiftR32ToBits val =
  insert vecshiftR32ImmField (vecshiftR32Imm val) 0

mkVecshiftR32 :: Word32 -> VecshiftR32
mkVecshiftR32 w =
  VecshiftR32 (extract vecshiftR32ImmField w)

vecshiftR32Operand :: OperandPayload
vecshiftR32Operand =
  OperandPayload { opTypeT = [t| VecshiftR32 |]
                 , opConE  = Just (varE 'mkVecshiftR32)
                 , opWordE = Just (varE 'vecshiftR32ToBits)
                 }

data VecshiftR8 = VecshiftR8 { vecshiftR8Imm :: W.W 3
                             } deriving (Eq, Ord, Show)

instance PP.Pretty VecshiftR8 where
  pPrint _ = PP.text "VecshiftR8: not implemented"

instance A.Arbitrary VecshiftR8 where
  arbitrary g = VecshiftR8 <$> A.arbitrary g

vecshiftR8ImmField :: Field 3
vecshiftR8ImmField = field 0

vecshiftR8ToBits :: VecshiftR8 -> Word32
vecshiftR8ToBits val =
  insert vecshiftR8ImmField (vecshiftR8Imm val) 0

mkVecshiftR8 :: Word32 -> VecshiftR8
mkVecshiftR8 w =
  VecshiftR8 (extract vecshiftR8ImmField w)

vecshiftR8Operand :: OperandPayload
vecshiftR8Operand =
  OperandPayload { opTypeT = [t| VecshiftR8 |]
                 , opConE  = Just (varE 'mkVecshiftR8)
                 , opWordE = Just (varE 'vecshiftR8ToBits)
                 }

data I32imm = I32imm { i32immImm :: W.W 4
                     } deriving (Eq, Ord, Show)

instance PP.Pretty I32imm where
  pPrint _ = PP.text "I32imm: not implemented"

instance A.Arbitrary I32imm where
  arbitrary g = I32imm <$> A.arbitrary g

i32immImmField :: Field 4
i32immImmField = field 0

i32immToBits :: I32imm -> Word32
i32immToBits val =
  insert i32immImmField (i32immImm val) 0

mkI32imm :: Word32 -> I32imm
mkI32imm w =
  I32imm (extract i32immImmField w)

i32immOperand :: OperandPayload
i32immOperand =
  OperandPayload { opTypeT = [t| I32imm |]
                 , opConE  = Just (varE 'mkI32imm)
                 , opWordE = Just (varE 'i32immToBits)
                 }

data Imm0255 = Imm0255 { imm0255Imm :: W.W 8
                       } deriving (Eq, Ord, Show)

instance PP.Pretty Imm0255 where
  pPrint _ = PP.text "Imm0255: not implemented"

instance A.Arbitrary Imm0255 where
  arbitrary g = Imm0255 <$> A.arbitrary g

imm0255ImmField :: Field 8
imm0255ImmField = field 0

imm0255ToBits :: Imm0255 -> Word32
imm0255ToBits val =
  insert imm0255ImmField (imm0255Imm val) 0

mkImm0255 :: Word32 -> Imm0255
mkImm0255 w =
  Imm0255 (extract imm0255ImmField w)

imm0255Operand :: OperandPayload
imm0255Operand =
  OperandPayload { opTypeT = [t| Imm0255 |]
                 , opConE  = Just (varE 'mkImm0255)
                 , opWordE = Just (varE 'imm0255ToBits)
                 }

data LogicalVecHwShift = LogicalVecHwShift { logicalVecHwShiftVal :: W.W 1
                                           } deriving (Eq, Ord, Show)

instance PP.Pretty LogicalVecHwShift where
  pPrint _ = PP.text "LogicalVecHwShift: not implemented"

instance A.Arbitrary LogicalVecHwShift where
  arbitrary g = LogicalVecHwShift <$> A.arbitrary g

logicalVecHwShiftValField :: Field 1
logicalVecHwShiftValField = field 0

logicalVecHwShiftToBits :: LogicalVecHwShift -> Word32
logicalVecHwShiftToBits val =
  insert logicalVecHwShiftValField (logicalVecHwShiftVal val) 0

mkLogicalVecHwShift :: Word32 -> LogicalVecHwShift
mkLogicalVecHwShift w =
  LogicalVecHwShift (extract logicalVecHwShiftValField w)

logicalVecHwShiftOperand :: OperandPayload
logicalVecHwShiftOperand =
  OperandPayload { opTypeT = [t| LogicalVecHwShift |]
                 , opConE  = Just (varE 'mkLogicalVecHwShift)
                 , opWordE = Just (varE 'logicalVecHwShiftToBits)
                 }

data LogicalVecShift = LogicalVecShift { logicalVecShiftVal :: W.W 2
                                       } deriving (Eq, Ord, Show)

instance PP.Pretty LogicalVecShift where
  pPrint _ = PP.text "LogicalVecShift: not implemented"

instance A.Arbitrary LogicalVecShift where
  arbitrary g = LogicalVecShift <$> A.arbitrary g

logicalVecShiftValField :: Field 2
logicalVecShiftValField = field 0

logicalVecShiftToBits :: LogicalVecShift -> Word32
logicalVecShiftToBits val =
  insert logicalVecShiftValField (logicalVecShiftVal val) 0

mkLogicalVecShift :: Word32 -> LogicalVecShift
mkLogicalVecShift w =
  LogicalVecShift (extract logicalVecShiftValField w)

logicalVecShiftOperand :: OperandPayload
logicalVecShiftOperand =
  OperandPayload { opTypeT = [t| LogicalVecShift |]
                 , opConE  = Just (varE 'mkLogicalVecShift)
                 , opWordE = Just (varE 'logicalVecShiftToBits)
                 }

data MoveVecShift = MoveVecShift { moveVecShiftVal :: W.W 1
                                 } deriving (Eq, Ord, Show)

instance PP.Pretty MoveVecShift where
  pPrint _ = PP.text "MoveVecShift: not implemented"

instance A.Arbitrary MoveVecShift where
  arbitrary g = MoveVecShift <$> A.arbitrary g

moveVecShiftValField :: Field 1
moveVecShiftValField = field 0

moveVecShiftToBits :: MoveVecShift -> Word32
moveVecShiftToBits val =
  insert moveVecShiftValField (moveVecShiftVal val) 0

mkMoveVecShift :: Word32 -> MoveVecShift
mkMoveVecShift w =
  MoveVecShift (extract moveVecShiftValField w)

moveVecShiftOperand :: OperandPayload
moveVecShiftOperand =
  OperandPayload { opTypeT = [t| MoveVecShift |]
                 , opConE  = Just (varE 'mkMoveVecShift)
                 , opWordE = Just (varE 'moveVecShiftToBits)
                 }

data VecshiftR16Narrow = VecshiftR16Narrow { vecshiftR16NarrowImm :: W.W 3
                                           } deriving (Eq, Ord, Show)

instance PP.Pretty VecshiftR16Narrow where
  pPrint _ = PP.text "VecshiftR16Narrow: not implemented"

instance A.Arbitrary VecshiftR16Narrow where
  arbitrary g = VecshiftR16Narrow <$> A.arbitrary g

vecshiftR16NarrowImmField :: Field 3
vecshiftR16NarrowImmField = field 0

vecshiftR16NarrowToBits :: VecshiftR16Narrow -> Word32
vecshiftR16NarrowToBits val =
  insert vecshiftR16NarrowImmField (vecshiftR16NarrowImm val) 0

mkVecshiftR16Narrow :: Word32 -> VecshiftR16Narrow
mkVecshiftR16Narrow w =
  VecshiftR16Narrow (extract vecshiftR16NarrowImmField w)

vecshiftR16NarrowOperand :: OperandPayload
vecshiftR16NarrowOperand =
  OperandPayload { opTypeT = [t| VecshiftR16Narrow |]
                 , opConE  = Just (varE 'mkVecshiftR16Narrow)
                 , opWordE = Just (varE 'vecshiftR16NarrowToBits)
                 }

data VecshiftR32Narrow = VecshiftR32Narrow { vecshiftR32NarrowImm :: W.W 4
                                           } deriving (Eq, Ord, Show)

instance PP.Pretty VecshiftR32Narrow where
  pPrint _ = PP.text "VecshiftR32Narrow: not implemented"

instance A.Arbitrary VecshiftR32Narrow where
  arbitrary g = VecshiftR32Narrow <$> A.arbitrary g

vecshiftR32NarrowImmField :: Field 4
vecshiftR32NarrowImmField = field 0

vecshiftR32NarrowToBits :: VecshiftR32Narrow -> Word32
vecshiftR32NarrowToBits val =
  insert vecshiftR32NarrowImmField (vecshiftR32NarrowImm val) 0

mkVecshiftR32Narrow :: Word32 -> VecshiftR32Narrow
mkVecshiftR32Narrow w =
  VecshiftR32Narrow (extract vecshiftR32NarrowImmField w)

vecshiftR32NarrowOperand :: OperandPayload
vecshiftR32NarrowOperand =
  OperandPayload { opTypeT = [t| VecshiftR32Narrow |]
                 , opConE  = Just (varE 'mkVecshiftR32Narrow)
                 , opWordE = Just (varE 'vecshiftR32NarrowToBits)
                 }

data VecshiftR64Narrow = VecshiftR64Narrow { vecshiftR64NarrowImm :: W.W 5
                                           } deriving (Eq, Ord, Show)

instance PP.Pretty VecshiftR64Narrow where
  pPrint _ = PP.text "VecshiftR64Narrow: not implemented"

instance A.Arbitrary VecshiftR64Narrow where
  arbitrary g = VecshiftR64Narrow <$> A.arbitrary g

vecshiftR64NarrowImmField :: Field 5
vecshiftR64NarrowImmField = field 0

vecshiftR64NarrowToBits :: VecshiftR64Narrow -> Word32
vecshiftR64NarrowToBits val =
  insert vecshiftR64NarrowImmField (vecshiftR64NarrowImm val) 0

mkVecshiftR64Narrow :: Word32 -> VecshiftR64Narrow
mkVecshiftR64Narrow w =
  VecshiftR64Narrow (extract vecshiftR64NarrowImmField w)

vecshiftR64NarrowOperand :: OperandPayload
vecshiftR64NarrowOperand =
  OperandPayload { opTypeT = [t| VecshiftR64Narrow |]
                 , opConE  = Just (varE 'mkVecshiftR64Narrow)
                 , opWordE = Just (varE 'vecshiftR64NarrowToBits)
                 }

