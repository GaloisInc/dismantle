module Dismantle.Tablegen (
  parseTablegen,
  filterISA,
  makeParseTables,
  module Dismantle.Tablegen.ISA,
  module Dismantle.Tablegen.Types
  ) where

import qualified GHC.Err.Located as L

import Control.Arrow ( (&&&) )
import Control.Monad ( guard )
import qualified Data.Array.Unboxed as UA
import Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI
import qualified Data.Foldable as F
import qualified Data.List.Split as L
import qualified Data.Map.Strict as M
import Data.Maybe ( mapMaybe )

import Dismantle.Tablegen.ISA
import Dismantle.Tablegen.Parser ( parseTablegen )
import Dismantle.Tablegen.Parser.Types
import Dismantle.Tablegen.Types
import qualified Dismantle.Tablegen.ByteTrie as BT

-- FIXME: Change byteTrie to not require a default value: instead,
-- just use another constructor in the ByteTrie type to represent "no
-- value".  Have lookup return Nothing if there is no value.

makeParseTables :: [InstructionDescriptor] -> Either BT.TrieError (BT.ByteTrie (Maybe InstructionDescriptor))
makeParseTables = BT.byteTrie Nothing . map (idMask &&& Just)

filterISA :: ISA -> Records -> [InstructionDescriptor]
filterISA isa = mapMaybe (instructionDescriptor isa) . tblDefs

toTrieBit :: Maybe BitRef -> BT.Bit
toTrieBit br =
  case br of
    Just (ExpectedBit b) -> BT.ExpectedBit b
    _ -> BT.Any

named :: String -> Named DeclItem -> Bool
named s n = namedName n == s

instructionDescriptor :: ISA -> Def -> Maybe InstructionDescriptor
instructionDescriptor isa def = do
  Named _ (FieldBits mbits) <- F.find (named "Inst") (defDecls def)
  Named _ (DagItem outs) <- F.find (named "OutOperandList") (defDecls def)
  Named _ (DagItem ins) <- F.find (named "InOperandList") (defDecls def)

  Named _ (StringItem ns) <- F.find (named "Namespace") (defDecls def)
  Named _ (StringItem decoder) <- F.find (named "DecoderNamespace") (defDecls def)
  Named _ (StringItem asmStr) <- F.find (named "AsmString") (defDecls def)
  Named _ (BitItem b) <- F.find (named "isPseudo") (defDecls def)
  let i = InstructionDescriptor { idMask = map toTrieBit mbits
                                , idMnemonic = defName def
                                , idNamespace = ns
                                , idDecoder = decoder
                                , idAsmString = asmStr
                                , idFields = fieldDescriptors isa (defName def) ins outs mbits
                                , idPseudo = b
                                }
  guard (isaInstructionFilter isa i)
  return i

fieldDescriptors :: ISA -> String -> SimpleValue -> SimpleValue -> [Maybe BitRef] -> [FieldDescriptor]
fieldDescriptors isa iname ins outs bits = map toFieldDescriptor (M.toList groups)
  where
    groups = foldr addBit M.empty (zip [0..] bits)
    inputFields = dagVarRefs iname "ins" ins
    outputFields = dagVarRefs iname "outs" outs

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
          (ty, dir) = fieldMetadata isa inputFields outputFields fldName
      in FieldDescriptor { fieldName = fldName
                         , fieldDirection = dir
                         , fieldType = ty
                         , fieldBits = UA.array (0, length bitPositions - 1) arrVals
                         }

-- toFieldType :: String -> FieldType
-- toFieldType s =
--   case s of
--     "gprc" -> Register
--     "pred" -> Predication
--     "i32imm" -> Immediate
--     _ -> L.error ("Unexpected field type: " ++ s)

fieldMetadata :: ISA -> M.Map (CI String) String -> M.Map (CI String) String -> String -> (FieldType, RegisterDirection)
fieldMetadata isa ins outs name =
  let cin = CI.mk name
  in case (M.lookup cin ins, M.lookup cin outs) of
    (Just kIn, Just kOut)
      | kIn == kOut -> (isaFieldType isa kIn, Both)
      | otherwise -> L.error ("Field type mismatch for in vs. out: " ++ show name)
    (Just kIn, Nothing) -> (isaFieldType isa kIn, In)
    (Nothing, Just kOut) -> (isaFieldType isa kOut, Out)
    -- FIXME: This might or might not be true.. need to look at more
    -- cases
    (Nothing, Nothing) -> (Immediate, In) -- L.error ("No field type for " ++ name)

dagVarRefs :: String
           -> String
           -- ^ The Dag head operator (e.g., "ins" or "outs")
           -> SimpleValue
           -> M.Map (CI String) String
dagVarRefs iname expectedOperator v =
  case v of
    VDag (DagArg (Identifier hd) _) args
      | hd == expectedOperator -> foldr argVarName M.empty args
    _ -> L.error ("Unexpected SimpleValue while looking for dag head " ++ expectedOperator ++ ": " ++ show v)
  where
    argVarName a m =
      case a of
        DagArg (Identifier i) _
          | [klass,var] <- L.splitOn ":" i -> M.insert (CI.mk var) klass m
          | i == "variable_ops" -> m -- See Note [variable_ops] 
        _ -> L.error ("Unexpected variable reference in a DAG for " ++ iname ++ ": " ++ show a)

{- Note [variable_ops]

Sparc has a call instruction (at least) that lists "variable_ops" as
input operands.  This virtual operand doesn't have a type annotation,
so fails our first condition that tries to find the type.  We don't
need to make an entry for it because no operand actually goes by that
name.

-}
