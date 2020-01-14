{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Dismantle.ARM.ISA (
  isa,
  isARM
  ) where

import           GHC.TypeLits ( KnownNat, Nat, natVal )

import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.Split as L
import           Data.Proxy ( Proxy(..) )
import           Data.Word ( Word32 )
import qualified Data.Word.Indexed as W
import qualified Language.Haskell.TH as TH
import qualified Text.XML.Light as X
import qualified Text.PrettyPrint.HughesPJClass as PP
import           Text.Printf (printf)

import qualified Data.Word.Indexed as I
import qualified Dismantle.ARM as DA

import Numeric (showHex)

asWord32 :: LBS.ByteString -> Word32
asWord32 bs = BG.runGet BG.getWord32le bs

fromWord32 :: Word32 -> LBS.ByteString
fromWord32 w = BP.runPut (BP.putWord32le w)

isa :: String -> DA.ISA
isa archName = DA.ISA { DA.isaName = archName
                      , DA.isaTgenBitPreprocess = id
                      , DA.isaInputEndianness = DA.Little LBS.reverse (concat . reverse . L.chunksOf 8)
                      , DA.isaUnusedBitsPolicy = Nothing
                      , DA.isaInstructionFilter = error "isaInstructionFilter is not used for XML"
                      , DA.isaPseudoInstruction = const False
                      , DA.isaOperandPayloadTypes = operandPayloadTypes
                      , DA.isaIgnoreOperand = const False
                      , DA.isaFormOverrides = []
                      , DA.isaPrettyOverrides = []
                      , DA.isaInsnWordFromBytes = 'asWord32
                      , DA.isaInsnWordToBytes = 'fromWord32
                      , DA.isaInsnAssembleType = ''Word32
                      , DA.isaMapOperandPayloadType = id
                      , DA.isaDefaultPrettyVariableValues = []
                      }

isARM :: X.Element -> Bool
isARM _ = True
-- isARM iclass = case X.findAttr (X.QName "isa" Nothing Nothing) iclass of
--   Nothing -> error $ "BUG: iclass missing \"isa\" attribute: \n" ++ show iclass
--   Just isaStr -> isaStr == "A32"

-- | In the XML specs, all fields are unsigned bitvectors (which are sometimes
-- treated as signed)
operandPayloadTypes :: [(String, DA.OperandPayload)]
operandPayloadTypes =
  (map (\sz -> (printf "Bv%d" sz, unsignedImmediate sz)) [1..24])
  ++
  (map (\sz -> (printf "QuasiMask%d" sz, quasimaskop sz)) [1..16])
  ++
  [ ("GPR3", gpRegister 3)
  , ("GPR4", gpRegister 4)
  , ("GPR4_1", gpRegister1 4)
  , ("SIMD2", simdReg 2)
  , ("SIMD3", simdReg 3)
  , ("SIMD4", simdReg 4)
  , ("SIMD5", simdReg 5)
  , ("SIMD7", simdReg 7)
  , ("SIMD5_1", simdReg1 5)
  ]
  where
    unsignedImmediate :: Integer -> DA.OperandPayload
    unsignedImmediate sz = DA.OperandPayload { DA.opTypeT = [t| I.W $(return (TH.LitT (TH.NumTyLit sz))) |]
                                            , DA.opConE = Just [| I.w |]
                                            , DA.opWordE = Just [| fromIntegral . I.unW |]
                                            }
    gpRegister :: Integer -> DA.OperandPayload
    gpRegister sz = DA.OperandPayload { DA.opTypeT = [t| GPR $(return (TH.LitT (TH.NumTyLit sz))) |]
                                      , DA.opConE = Just (TH.varE 'gpr)
                                      , DA.opWordE = Just [| fromIntegral . W.unW . unGPR |]
                                      }

    gpRegister1 :: Integer -> DA.OperandPayload
    gpRegister1 sz = DA.OperandPayload { DA.opTypeT = [t| GPR1 $(return (TH.LitT (TH.NumTyLit sz))) |]
                                       , DA.opConE = Just (TH.varE 'gpr1)
                                       , DA.opWordE = Just [| fromIntegral . W.unW . unGPR1 |]
                                       }

    simdReg :: Integer -> DA.OperandPayload
    simdReg sz = DA.OperandPayload { DA.opTypeT = [t| SIMD $(return (TH.LitT (TH.NumTyLit sz))) |]
                                   , DA.opConE = Just (TH.varE 'simd)
                                   , DA.opWordE = Just [| fromIntegral . W.unW . unSIMD |]
                                   }

    simdReg1 :: Integer -> DA.OperandPayload
    simdReg1 sz = DA.OperandPayload { DA.opTypeT = [t| SIMD1 $(return (TH.LitT (TH.NumTyLit sz))) |]
                                    , DA.opConE = Just (TH.varE 'simd1)
                                    , DA.opWordE = Just [| fromIntegral . W.unW . unSIMD1 |]
                                    }

    quasimaskop :: Integer -> DA.OperandPayload
    quasimaskop sz = DA.OperandPayload { DA.opTypeT = [t| QuasiMask $(return (TH.LitT (TH.NumTyLit sz))) |]
                                       , DA.opConE = Just (TH.varE 'quasimask)
                                       , DA.opWordE = Just [| fromIntegral . W.unW . unQuasiMask |]
                                       }

-- | General-purpose register by number
newtype GPR n = GPR { unGPR :: W.W n }
  deriving (Eq, Ord, Show)

gpr :: KnownNat n => Word32 -> GPR n
gpr = GPR . fromIntegral

instance KnownNat n => PP.Pretty (GPR n) where
  pPrint (GPR w) = PP.text "GPR: " <> PP.pPrint w

-- | General-purpose register by number - representing the given register offset by one
newtype GPR1 n = GPR1 { unGPR1 :: W.W n }
  deriving (Eq, Ord, Show)

gpr1 :: KnownNat n => Word32 -> GPR1 n
gpr1 = GPR1 . fromIntegral

instance KnownNat n => PP.Pretty (GPR1 n) where
  pPrint (GPR1 w) = PP.text "GPR1: " <> PP.pPrint w

-- | SIMD/FP register by number

newtype SIMD n = SIMD { unSIMD :: W.W n }
  deriving (Eq, Ord, Show)

simd :: KnownNat n => Word32 -> SIMD n
simd = SIMD . fromIntegral

instance KnownNat n => PP.Pretty (SIMD n) where
  pPrint (SIMD w) = PP.text "SIMD: " <> PP.pPrint w

newtype SIMD1 n = SIMD1 { unSIMD1 :: W.W n }
  deriving (Eq, Ord, Show)

simd1 :: KnownNat n => Word32 -> SIMD1 n
simd1 = SIMD1 . fromIntegral


instance KnownNat n => PP.Pretty (SIMD1 n) where
  pPrint (SIMD1 w) = PP.text "SIMD1: " <> PP.pPrint w


newtype QuasiMask n = QuasiMask { unQuasiMask :: W.W n }
  deriving (Eq, Ord, Show)

quasimask :: KnownNat n => Word32 -> QuasiMask n
quasimask = QuasiMask . fromIntegral


instance KnownNat n => PP.Pretty (QuasiMask n) where
  pPrint (QuasiMask w) = PP.text "QuasiMask(" <> PP.pPrint (W.width w) <> PP.text "): " <> PP.pPrint w
