module Dismantle.ASL (
  loadASL,
  DT.ISA(..),
  DT.Endianness(..),
  DT.OperandPayload(..),
  DT.FormOverride(..),
  DT.InstFieldDescriptor(..),
  DT.UnusedBitsPolicy(..)
  ) where

import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Word ( Word8 )
import           Text.Printf ( printf )
import qualified Language.ASL.Syntax as AS
-- import qualified Language.ASL.Parser as AP

import qualified Dismantle.Tablegen as DT
import qualified Dismantle.Tablegen.ByteTrie as BT
import qualified Dismantle.Tablegen.Parser.Types as PT

loadASL :: String -> [AS.Instruction] -> DT.ISADescriptor
loadASL arch insns =
  DT.ISADescriptor { DT.isaInstructions = instrs
                   , DT.isaOperands = S.toList (S.fromList (concatMap instrOperandTypes instrs))
                   , DT.isaErrors = []
                   }
  where
    instrs = concatMap (aslToInsnDesc arch) insns

instrOperandTypes :: DT.InstructionDescriptor -> [DT.OperandType]
instrOperandTypes idesc = map DT.opType (DT.idInputOperands idesc ++ DT.idOutputOperands idesc)

aslToInsnDesc :: String -> AS.Instruction -> [DT.InstructionDescriptor]
aslToInsnDesc arch i = map (encodingToInstDesc arch) (AS.instEncodings i)

encodingToInstDesc :: String -> AS.InstructionEncoding -> DT.InstructionDescriptor
encodingToInstDesc arch e =
  DT.InstructionDescriptor { DT.idMask = fixedEncodingMask e
                           , DT.idMnemonic = T.unpack (AS.encName e)
                           , DT.idInputOperands = map toOperandDescriptor (AS.encFields e)
                           , DT.idOutputOperands = [] -- See Note [Output Operands]
                           , DT.idNamespace = arch
                           , DT.idDecoderNamespace = show (AS.encInstrSet e)
                           , DT.idAsmString = asmString e
                           , DT.idPseudo = False
                           , DT.idDefaultPrettyVariableValues = []
                           , DT.idPrettyVariableOverrides = []
                           }

-- | The ASL specs don't have any information about textual encodings of
-- instructions.  We'll have to make something up here.
--
-- Ideally, our made up encodings will be close enough and just have one
-- variable slot for each field, in the order presented.  We'll also have to
-- build a translator from the fully elaborated names into shorter mnemonics
-- (i.e., dropping the aarch_ prefix and any suffixes)
asmString :: AS.InstructionEncoding -> String
asmString = undefined

fixedEncodingMask :: AS.InstructionEncoding -> [BT.Bit]
fixedEncodingMask e = map toBTBit (AS.encOpcodeMask e)
  where
    toBTBit mb =
      case mb of
        AS.MaskBitSet -> BT.ExpectedBit True
        AS.MaskBitUnset -> BT.ExpectedBit False
        AS.MaskBitEither -> BT.Any

-- | For ASL, all fields are of type BV[N] (i.e., bitvectors of length N)
toOperandDescriptor :: AS.InstructionField -> DT.OperandDescriptor
toOperandDescriptor fld =
  DT.OperandDescriptor { DT.opName = T.unpack (AS.instFieldName fld)
                       , DT.opChunks = fieldToChunks fld
                       , DT.opType = fieldToType fld
                       }

-- | The operand descriptor allows operand bits to be scattered arbitrarily into
-- "chunks" within an encoded instruction.  The ARM ISAs don't use that (unless
-- they explicitly break a larger operand into multiple fields), so every field
-- will be converted into a single chunk.
fieldToChunks :: AS.InstructionField -> [(DT.IBit, PT.OBit, Word8)]
fieldToChunks fld = [( DT.IBit (fromIntegral (AS.instFieldBegin fld))
                     , PT.OBit 0
                     , fromIntegral (AS.instFieldOffset fld)
                     )]

fieldToType :: AS.InstructionField -> DT.OperandType
fieldToType fld = DT.OperandType (printf "bv%d" (AS.instFieldOffset fld))

{- Note [Output Operands]

Unlike the Tablegen data, the ASL specs don't have an explicit notion of output
operands (i.e., operands that are written but not read).  We don't really need
that in the decoder, though we could compute it by looking at the semantics.
For now, we are just treating all inputs as input operands.

-}
