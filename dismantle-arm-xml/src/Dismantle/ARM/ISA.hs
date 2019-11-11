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
import qualified Language.Haskell.TH as TH
import qualified Text.XML.Light as X

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
                      , DA.isaInputEndianness = DA.Little id id -- LBS.reverse (concat . reverse . L.chunksOf 8)
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
  [ ("Bv1", unsignedImmediate (Proxy @1))
  , ("Bv2", unsignedImmediate (Proxy @2))
  , ("Bv3", unsignedImmediate (Proxy @3))
  , ("Bv4", unsignedImmediate (Proxy @4))
  , ("Bv5", unsignedImmediate (Proxy @5))
  , ("Bv6", unsignedImmediate (Proxy @6))
  , ("Bv7", unsignedImmediate (Proxy @7))
  , ("Bv8", unsignedImmediate (Proxy @8))
  , ("Bv10", unsignedImmediate (Proxy @10))
  , ("Bv11", unsignedImmediate (Proxy @11))
  , ("Bv12", unsignedImmediate (Proxy @12))
  , ("Bv14", unsignedImmediate (Proxy @14))
  , ("Bv15", unsignedImmediate (Proxy @15))
  , ("Bv16", unsignedImmediate (Proxy @16))
  , ("Bv24", unsignedImmediate (Proxy @24))
  ]
  where
    unsignedImmediate :: forall (n :: Nat) . (KnownNat n) => Proxy n -> DA.OperandPayload
    unsignedImmediate p = DA.OperandPayload { DA.opTypeT = [t| I.W $(return (TH.LitT (TH.NumTyLit (natVal p)))) |]
                                            , DA.opConE = Just [| I.w |]
                                            , DA.opWordE = Just [| fromIntegral . I.unW |]
                                            }

