module Dismantle.Tablegen (
  parseTablegen,
  filterISA,
  makeParseTables,
  parseInstruction,
  Parser(..),
  module Dismantle.Tablegen.ISA,
  module Dismantle.Tablegen.Types
  ) where

import qualified GHC.Err.Located as L

import Control.Applicative
import Control.Arrow ( (&&&) )
import Control.Monad ( guard )
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI
import qualified Data.Foldable as F
import qualified Data.List.Split as L
import qualified Data.Map.Strict as M
import Data.Maybe ( fromMaybe, mapMaybe )
import qualified Data.Set as S

import Dismantle.Tablegen.ISA
import Dismantle.Tablegen.Parser ( parseTablegen )
import Dismantle.Tablegen.Parser.Types
import Dismantle.Tablegen.Types
import qualified Dismantle.Tablegen.ByteTrie as BT

data Parser a = Parser (LBS.ByteString -> a)

parseInstruction :: BT.ByteTrie (Maybe (Parser a)) -> LBS.ByteString -> (Int, Maybe a)
parseInstruction trie0 bs0 = go bs0 trie0 bs0 0
  where
    go bs1 trie bs consumed =
      case LBS.uncons bs of
        Nothing -> (consumed, Nothing)
        Just (b, bs') ->
          case BT.lookupByte trie b of
            Left next -> go bs1 next bs' (consumed + 1)
            Right Nothing -> (consumed + 1, Nothing)
            Right (Just (Parser p)) -> (consumed + 1, Just (p bs1))

makeParseTables :: ISA -> ISADescriptor -> Either BT.TrieError (BT.ByteTrie (Maybe InstructionDescriptor))
makeParseTables isa = BT.byteTrie Nothing . map (idMask &&& Just) . filter (not . isaPseudoInstruction isa) . isaInstructions

filterISA :: ISA -> Records -> ISADescriptor
filterISA isa rs =
  ISADescriptor { isaInstructions = insns
                , isaRegisterClasses = registerClasses
                , isaRegisters = registerOperands
                , isaOperands = S.toList operandTypes
                }
  where
    insns = mapMaybe (instructionDescriptor isa) $ tblDefs rs
    dagOperands = filter isDAGOperand $ tblDefs rs
    registerClasses = map (RegisterClass . defName) $ filter isRegisterClass dagOperands
    registerOperands = mapMaybe isRegisterOperand dagOperands
    operandTypes = foldr extractOperands S.empty insns

extractOperands :: InstructionDescriptor -> S.Set OperandType -> S.Set OperandType
extractOperands i s = foldr extractOperandTypes s (idInputOperands i ++ idOutputOperands i)

extractOperandTypes :: OperandDescriptor -> S.Set OperandType -> S.Set OperandType
extractOperandTypes f = S.insert (opType f)

isRegisterClass :: Def -> Bool
isRegisterClass def = Metadata "RegisterClass" `elem` defMetadata def

isRegisterOperand :: Def -> Maybe (String, RegisterClass)
isRegisterOperand def = do
  guard (Metadata "RegisterOperand" `elem` defMetadata def)
  Named _ (ClassItem cname) <- F.find (named "RegClass") (defDecls def)
  return (defName def, RegisterClass cname)

isDAGOperand :: Def -> Bool
isDAGOperand d = Metadata "DAGOperand" `elem` defMetadata d

toTrieBit :: Maybe BitRef -> BT.Bit
toTrieBit br =
  case br of
    Just (ExpectedBit b) -> BT.ExpectedBit b
    _ -> BT.Any

named :: String -> Named DeclItem -> Bool
named s n = namedName n == s

-- FIXME: We'll need to do something to ensure that all of the fields
-- of the instruction are accounted for by 'operandDescriptors'.
-- Right now, we can only pull out those fields mentioned in the DAG
-- descriptors.  I *believe* that should cover all operands, but have
-- no idea if that is true.
--
-- Moreover, quite a few operands are spelled slightly differently in
-- the DAG vs bit descriptors.
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
                                , idInputOperands = operandDescriptors isa (defName def) "ins" ins mbits
                                , idOutputOperands = operandDescriptors isa (defName def) "outs" outs mbits
--                                , idFields = fieldDescriptors isa (defName def) ins outs mbits
                                , idPseudo = or [ b -- See Note [Pseudo Instructions]
                                                , cgOnly
                                                , asmParseOnly
                                                , Metadata "Pseudo" `elem` defMetadata def
                                                ]
                                }
  guard (isaInstructionFilter isa i)
  return i

-- | Extract OperandDescriptors from the bits pattern, given type
-- information from the InOperandList or OutOperandList DAG items.
--
-- The important thing is that we are preserving the *order* of
-- operands here so that users have a chance of making sense of the
-- generated instructions.
operandDescriptors :: ISA
                   -> String
                   -- ^ Instruction mnemonic
                   -> String
                   -- ^ DAG operator string ("ins" or "outs")
                   -> SimpleValue
                   -- ^ The DAG item (either InOperandList or OutOperandList)
                   -> [Maybe BitRef]
                   -- ^ The bits descriptor so that we can find the bit numbers for the field
                   -> [OperandDescriptor]
operandDescriptors isa mnemonic dagOperator dagItem bits =
  case dagItem of
    VDag (DagArg (Identifier hd) _) args
      | hd == dagOperator -> mapMaybe parseOperand args
    _ -> L.error ("Unexpected SimpleValue while looking for DAG head " ++ dagOperator ++ ": " ++ show dagItem)

  where
    -- A map of field names (derived from the 'Inst' field of a
    -- definition) to pairs of (instructionIndex, operandIndex), where
    -- the instruction index is the bit number in the instruction and
    -- the operand index is the index into the operand of that bit.
    operandBits :: M.Map (CI String) [(Int, Int)]
    operandBits = foldr addBit M.empty (zip [0..] bits)

    addBit (bitNum, mbit) m =
      case mbit of
        Just (FieldBit fldName fldIdx) ->
          M.insertWith (++) (CI.mk fldName) [(bitNum, fldIdx)] m
        _ -> m

    parseOperand :: DagArg -> Maybe OperandDescriptor
    parseOperand arg =
      case arg of
        DagArg (Identifier i) _
          | [klass, var] <- L.splitOn ":$" i ->
            let err = L.error ("No field bits found for field " ++ var ++ " while parsing instruction " ++ mnemonic)
                bitPositions = fromMaybe err $ lookupFieldBits var
                arrVals = [ (fldIdx, fromIntegral bitNum)
                          | (bitNum, fldIdx) <- bitPositions
                          ]
                bitRange = findFieldBitRange bitPositions
            in Just OperandDescriptor { opName = var
                                      , opType = OperandType klass
                                      , opBits = UA.array bitRange arrVals
                                      }
          | i == "variable_ops" ->
            -- This case is expected sometimes - there is no single
            -- argument to make (yet, we might add one)
            Nothing
        _ -> L.error ("Unexpected variable reference in a DAG for " ++ mnemonic ++ ": " ++ show arg)

    lookupFieldBits :: String -> Maybe [(Int, Int)]
    lookupFieldBits fldName =
      case M.lookup (CI.mk fldName) operandBits of
        Just fldBits -> Just fldBits
        Nothing ->
          let err = L.error ("No field bits found for field " ++ fldName ++ " while parsing instruction " ++ mnemonic)
          in case isaOperandClassMapping isa fldName of
            [] -> Nothing -- err
            alternatives -> foldr (<|>) err (map lookupFieldBits alternatives)

-- | Find the actual length of a field.
--
-- The bit positions tell us which bits are encoded in the
-- instruction, but some values have implicit bits that are not
-- actually in the instruction.
findFieldBitRange :: [(Int, Int)] -> (Int, Int)
findFieldBitRange bitPositions = (minimum (map snd bitPositions), maximum (map snd bitPositions))

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
