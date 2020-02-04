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
import qualified Dismantle.Tablegen as DA

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
  where
    unsignedImmediate :: Integer -> DA.OperandPayload
    unsignedImmediate sz = DA.OperandPayload { DA.opTypeT = [t| I.W $(return (TH.LitT (TH.NumTyLit sz))) |]
                                            , DA.opConE = Just [| I.w |]
                                            , DA.opWordE = Just [| fromIntegral . I.unW |]
                                            }

    quasimaskop :: Integer -> DA.OperandPayload
    quasimaskop sz = DA.OperandPayload { DA.opTypeT = [t| QuasiMask $(return (TH.LitT (TH.NumTyLit sz))) |]
                                       , DA.opConE = Just (TH.varE 'quasimask)
                                       , DA.opWordE = Just [| fromIntegral . W.unW . unQuasiMask |]
                                       }
-- | QuasiMask / Psuedo-operand

newtype QuasiMask n = QuasiMask { unQuasiMask :: W.W n }
  deriving (Eq, Ord, Show)

quasimask :: KnownNat n => Word32 -> QuasiMask n
quasimask = QuasiMask . fromIntegral


instance KnownNat n => PP.Pretty (QuasiMask n) where
  pPrint (QuasiMask w) = PP.text "QuasiMask" <> PP.pPrint "(" <> PP.pPrint (W.width w) <> PP.text "): " <> PP.pPrint w
