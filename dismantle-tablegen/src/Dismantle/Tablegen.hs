module Dismantle.Tablegen (
  parseTablegen,
  makeParseTables
  ) where

import Control.Monad ( guard )
import qualified Data.Array.Unboxed as UA
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Data.Maybe ( mapMaybe )
import Data.Word ( Word8 )

import Dismantle.Tablegen.Parser ( parseTablegen )
import Dismantle.Tablegen.Types
import qualified Dismantle.Tablegen.ByteTrie as BT

makeParseTables :: Records -> Either BT.TrieError (BT.ByteTrie InstructionDescriptor)
makeParseTables = BT.byteTrie undefined . mapMaybe (toByteSpec (const True)) . tblDefs

toByteSpec :: (InstructionDescriptor -> Bool) -> Def -> Maybe ([BT.Bit], InstructionDescriptor)
toByteSpec p def = do
  desc <- instructionDescriptor def
  guard (p desc)
  return (idMask desc, desc)

toTrieBit :: Maybe BitRef -> BT.Bit
toTrieBit br =
  case br of
    Just (ExpectedBit b) -> BT.ExpectedBit b
    _ -> BT.Any

named :: String -> Named DeclItem -> Bool
named s n = namedName n == s

instructionDescriptor :: Def -> Maybe InstructionDescriptor
instructionDescriptor def = do
  Named _ (FieldBits mbits) <- F.find (named "Inst") (defDecls def)
  Named _ (DagItem outs) <- F.find (named "OutOperandList") (defDecls def)
  Named _ (DagItem ins) <- F.find (named "InOperandList") (defDecls def)

  Named _ (StringItem ns) <- F.find (named "Namespace") (defDecls def)
  Named _ (StringItem decoder) <- F.find (named "DecoderNamespace") (defDecls def)
  Named _ (StringItem asmStr) <- F.find (named "AsmString") (defDecls def)
  return InstructionDescriptor { idMask = map toTrieBit mbits
                               , idMnemonic = defName def
                               , idNamespace = ns
                               , idDecoder = decoder
                               , idAsmString = asmStr
                               , idFields = fieldDescriptors mbits
                               }

fieldDescriptors :: [Maybe BitRef] -> [FieldDescriptor]
fieldDescriptors bits = map toFieldDescriptor (M.toList groups)
  where
    groups = foldr addBit M.empty (zip [0..] bits)
    addBit (bitNum, mbr) m =
      case mbr of
        Just (FieldBit fldName fldIdx) ->
          M.insertWith (++) fldName [(bitNum, fldIdx)] m
        _ -> m

    toFieldDescriptor :: (String, [(Int, Int)]) -> FieldDescriptor
    toFieldDescriptor (fldName, bitPositions) =
      let arrVals = [ (fldIdx, fromIntegral bitNum)
                    | (bitNum, fldIdx) <- bitPositions
                    ]
      in FieldDescriptor { fieldName = fldName
                         , fieldDirection = undefined
                         , fieldType = undefined
                         , fieldBits = UA.array (0, length bitPositions - 1) arrVals
                         }

-- | The direction of a field (input, output, or both)
data Direction = In | Out | Both
  deriving (Show)

-- | The type of data contained in a field operand.
--
-- This is designed to distinguish between register references,
-- immediates, and other types of values.
data FieldType = Immediate
               | Register
               | Offset
               deriving (Show)

-- | Description of an operand field in an instruction (could be a
-- register reference or an immediate)
data FieldDescriptor =
  FieldDescriptor { fieldName :: String
                  , fieldBits :: UA.UArray Int Word8
                  , fieldDirection :: Direction
                  , fieldType :: FieldType
                  }
  deriving (Show)

-- | Description of an instruction, abstracted from the tablegen
-- definition
data InstructionDescriptor =
  InstructionDescriptor { idMask :: [BT.Bit]
                        , idMnemonic :: String
                        , idFields :: [FieldDescriptor]
                        , idNamespace :: String
                        , idDecoder :: String
                        , idAsmString :: String
                        }
  deriving (Show)

