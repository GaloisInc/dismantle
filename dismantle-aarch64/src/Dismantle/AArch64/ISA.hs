{-# LANGUAGE TemplateHaskell #-}
module Dismantle.AArch64.ISA (
  isa
  ) where

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Set as S
import Data.Word ( Word8, Word16, Word32, Word64 )

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Dismantle.Tablegen.ISA
import Dismantle.Tablegen.Types
import Dismantle.Tablegen.Parser.Types
  ( Metadata(Metadata)
  , defName
  , defMetadata
  )
import qualified Dismantle.AArch64.Operands as AArch64

asWord32 :: LBS.ByteString -> Word32
asWord32 = B.runGet B.getWord32be

fromWord32 :: Word32 -> LBS.ByteString
fromWord32 = B.runPut . B.putWord32be

isa :: ISA
isa = ISA { isaName = "AArch64"
          , isaTgenBitPreprocess = id
          , isaInputEndianness = Big
          , isaInstructionFilter = aarch64Filter
          , isaPseudoInstruction = aarch64Pseudo
          , isaOperandPayloadTypes = aarch64OperandPayloadTypes
          , isaInsnWordFromBytes = 'asWord32
          , isaInsnWordToBytes = 'fromWord32
          , isaInsnAssembleType = ''Word32
          , isaIgnoreOperand = const False
          , isaFormOverrides = overrides
          , isaMapOperandPayloadType = id
          , isaDefaultPrettyVariableValues = []
          , isaPrettyOverrides = []
          , isaUnusedBitsPolicy = Nothing
          }
  where

    aarch64OperandPayloadTypes =
        [
        ]

    aarch64Filter = hasNamedString "Namespace" "AArch64" &&&
                    (not . isPseudo) &&&
                    (not . ignoredDef) &&&
                    (not . ignoredMetadata)

    aarch64Pseudo = idPseudo

    pseudoInstructionNames =
        [
        ]

    ignoredDef d = defName d `elem`
        [
        ]

    ignoredMetadataNames = S.fromList $ Metadata <$>
        [
        ]

    ignoredMetadata d = not $ S.null $
                        S.intersection (S.fromList $ defMetadata d)
                                       ignoredMetadataNames

    overrides =
        [
        ]
