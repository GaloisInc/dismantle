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

makeParseTables :: ISA -> [InstructionDescriptor] -> Either BT.TrieError (BT.ByteTrie (Maybe InstructionDescriptor))
makeParseTables isa = BT.byteTrie Nothing . map (idMask &&& Just) . filter (not . isaPseudoInstruction isa)

filterISA :: ISA -> Records -> ISADescriptor
filterISA isa rs =
  ISADescriptor { isaInstructions = mapMaybe (instructionDescriptor isa) $ tblDefs rs
                , isaRegisterClasses = registerClasses
                , isaRegisters = registerOperands
                }
  where
    dagOperands = filter isDAGOperand $ tblDefs rs
    registerClasses = map (RegisterClass . defName) $ filter isRegisterClass dagOperands
    registerOperands = mapMaybe isRegisterOperand dagOperands

isRegisterClass :: Def -> Bool
isRegisterClass def = Metadata "RegisterClass" `elem` defMetadata def

isRegisterOperand :: Def -> Maybe (String, RegisterClass)
isRegisterOperand def = do
  guard (Metadata "RegisterOperand" `elem` defMetadata def)
  Named _ (ClassItem cname) <- F.find (named "RegClass") (defDecls def)
  return (defName def, RegisterClass cname)

{-



-}

isDAGOperand :: Def -> Bool
isDAGOperand d = Metadata "DAGOperand" `elem` defMetadata d

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
  Named _ (BitItem cgOnly) <- F.find (named "isCodeGenOnly") (defDecls def)
  Named _ (BitItem asmParseOnly) <- F.find (named "isAsmParserOnly") (defDecls def)

  let i = InstructionDescriptor { idMask = map toTrieBit mbits
                                , idMnemonic = defName def
                                , idNamespace = ns
                                , idDecoder = decoder
                                , idAsmString = asmStr
                                , idFields = fieldDescriptors isa (defName def) ins outs mbits
                                , idPseudo = or [ b -- See Note [Pseudo Instructions]
                                                , cgOnly
                                                , asmParseOnly
                                                , Metadata "Pseudo" `elem` defMetadata def
                                                ]
                                }
  guard (isaInstructionFilter isa i)
  return i

fieldDescriptors :: ISA
                 -> String
                 -- ^ The instruction mnemonic
                 -> SimpleValue
                 -- ^ The "ins" DAG item (to let us identify instruction input types)
                 -> SimpleValue
                 -- ^ The "outs" DAG item (to let us identify instruction outputs)
                 -> [Maybe BitRef]
                 -- ^ The bits descriptor (so we can pick out fields)
                 -> [FieldDescriptor]
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
          (ty, dir) = fieldMetadata inputFields outputFields fldName
          fldRange = findFieldBitRange bitPositions
      in FieldDescriptor { fieldName = fldName
                         , fieldDirection = dir
                         , fieldType = ty
                         , fieldBits = UA.array fldRange arrVals
                         }

-- | Find the actual length of a field.
--
-- The bit positions tell us which bits are encoded in the
-- instruction, but some values have implicit bits that are not
-- actually in the instruction.
findFieldBitRange :: [(Int, Int)] -> (Int, Int)
findFieldBitRange bitPositions = (minimum (map snd bitPositions), maximum (map snd bitPositions))

fieldMetadata :: M.Map (CI String) String -> M.Map (CI String) String -> String -> (FieldType, RegisterDirection)
fieldMetadata ins outs name =
  let cin = CI.mk name
  in case (M.lookup cin ins, M.lookup cin outs) of
    (Just kIn, Just kOut)
      | kIn == kOut -> (FieldType kIn, Both)
      | otherwise -> L.error ("Field type mismatch for in vs. out: " ++ show name)
    (Just kIn, Nothing) -> (FieldType kIn, In)
    (Nothing, Just kOut) -> (FieldType kOut, Out)
    -- FIXME: This might or might not be true.. need to look at more
    -- cases
    (Nothing, Nothing) -> (FieldType "unknown", In)

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
          | [klass,var] <- L.splitOn ":$" i -> M.insert (CI.mk var) klass m
          | i == "variable_ops" -> m -- See Note [variable_ops]
        _ -> L.error ("Unexpected variable reference in a DAG for " ++ iname ++ ": " ++ show a)

{- Note [variable_ops]

Sparc has a call instruction (at least) that lists "variable_ops" as
input operands.  This virtual operand doesn't have a type annotation,
so fails our first condition that tries to find the type.  We don't
need to make an entry for it because no operand actually goes by that
name.

-}

{- Note [Pseudo Instructions]

We have an expanded definition of pseudo-instructions compared to
LLVM.  There are instructions that are pure pseudo instructions
(according to LLVM); these mostly represent high level things
that turn into function calls or don't exist at all.  There are
also "codegen only" and "assembly parser only" instructions that
are rewritten by the assembler or codegen into more primitive
instructions.  For the purposes of parsing, these don't need to
exist since they have alternate parses.

-}
