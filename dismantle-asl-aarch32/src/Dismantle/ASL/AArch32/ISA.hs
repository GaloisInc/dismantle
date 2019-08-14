{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Dismantle.ASL.AArch32.ISA (
  isa,
  isARM
  ) where

import           GHC.TypeLits ( KnownNat, Nat, natVal )

import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import qualified Data.ByteString.Lazy as LBS
import           Data.Proxy ( Proxy(..) )
import           Data.Word ( Word32 )
import qualified Language.Haskell.TH as TH

import qualified Language.ASL.Syntax as AS

import qualified Data.Word.Indexed as I
import qualified Dismantle.ASL as DA


asWord32 :: LBS.ByteString -> Word32
asWord32 = BG.runGet BG.getWord32be

fromWord32 :: Word32 -> LBS.ByteString
fromWord32 = BP.runPut . BP.putWord32be

isa :: DA.ISA
isa = DA.ISA { DA.isaName = "AARCH32"
             , DA.isaTgenBitPreprocess = id
             , DA.isaInputEndianness = DA.Little id id
             , DA.isaUnusedBitsPolicy = Nothing
             , DA.isaInstructionFilter = error "isaInstructionFilter is not used for ASL"
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


isARM :: AS.InstructionEncoding -> Bool
isARM e = AS.encInstrSet e == AS.A32

-- | In the ASL specs, all fields are unsigned bitvectors (which are sometimes
-- treated as signed)
operandPayloadTypes :: [(String, DA.OperandPayload)]
operandPayloadTypes =
  [ ("Bv1", unsignedImmediate (Proxy @1))
  , ("Bv2", unsignedImmediate (Proxy @2))
  , ("Bv3", unsignedImmediate (Proxy @3))
  , ("Bv4", unsignedImmediate (Proxy @4))
  , ("Bv5", unsignedImmediate (Proxy @5))
  , ("Bv6", unsignedImmediate (Proxy @6))
  , ("Bv8", unsignedImmediate (Proxy @8))
  , ("Bv12", unsignedImmediate (Proxy @12))
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
