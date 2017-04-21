{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Dismantle.Tablegen (
  parseTablegen,
  filterISA,
  parsableInstructions,
  parseInstruction,
  Parser(..),
  module Dismantle.Tablegen.ISA,
  module Dismantle.Tablegen.Types
  ) where

import qualified GHC.Err.Located as L

import Control.Monad ( guard, when )
import qualified Control.Monad.Cont as CC
import qualified Control.Monad.State.Strict as St
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NL
import qualified Data.List.Split as L
import qualified Data.Map.Strict as M
import Data.Maybe ( fromMaybe, mapMaybe )
import qualified Data.Set as S
import Data.Word ( Word8 )
import Text.Printf ( printf )

import Dismantle.Tablegen.ISA
import Dismantle.Tablegen.Parser ( parseTablegen )
import Dismantle.Tablegen.Parser.Types
import Dismantle.Tablegen.Types
import qualified Dismantle.Tablegen.ByteTrie as BT
import Debug.Trace
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

parsableInstructions :: ISA -> ISADescriptor -> [InstructionDescriptor]
parsableInstructions isa = filter (not . isaPseudoInstruction isa) . isaInstructions

data FilterState = FilterState { stErrors :: [(String, String)]
                               , stInsns :: [InstructionDescriptor]
                               }

-- Use the endianness setting in the ISA to do our byteswapping here, while translating records
filterISA :: ISA -> Records -> ISADescriptor
filterISA isa rs =
  ISADescriptor { isaInstructions = insns
                , isaRegisterClasses = registerClasses
                , isaRegisters = registerOperands
                , isaOperands = S.toList operandTypes
                , isaErrors = stErrors st1
                }
  where
    st0 = FilterState { stErrors = []
                      , stInsns = []
                      }
    st1 = St.execState (CC.runContT (runFilter (filterInstructions isa (tblDefs rs))) return) st0
    dagOperands = filter isDAGOperand $ tblDefs rs
    registerClasses = map (RegisterClass . defName) $ filter isRegisterClass dagOperands
    registerOperands = mapMaybe isRegisterOperand dagOperands
    insns = reverse $ stInsns st1
    operandTypes = foldr extractOperands S.empty insns


newtype FM a = FM { runFilter :: CC.ContT () (St.State FilterState) a }
  deriving (Functor,
            Monad,
            Applicative,
            CC.MonadCont,
            St.MonadState FilterState)

filterInstructions :: ISA -> [Def] -> FM ()
filterInstructions isa = mapM_ (toInstructionDescriptor isa)

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

-- | Try to extract the basic encoding information from the 'Def'.  If
-- it isn't available, skip the 'Def'.  If it is available, call the
-- continuation that will use it.
withEncoding :: Endianness
             -> Def
             -> (SimpleValue -> SimpleValue -> [Maybe BitRef] -> [Maybe BitRef] -> [String] -> FM ())
             -> FM ()
withEncoding endian def k =
  case mvals of
    Just (outs, ins, mbits, endianBits, ordFlds) -> k outs ins mbits endianBits ordFlds
    Nothing -> return ()
  where
    mvals = do
      Named _ (FieldBits mbits) <- F.find (named "Inst") (defDecls def)
      -- Inst has all of the names of the fields embedded in the
      -- instruction.  We can collect all of those names into a set.
      -- Then we can filter *all* of the defs according to that set,
      -- maintaining the order of the fields.  We need to return that
      -- here so that we can map fields to DAG items, which tell us
      -- types and direction.
      let fields = foldr addFieldName S.empty mbits
          orderedFieldDefs = mapMaybe (isOperandField fields) (defDecls def)
      Named _ (DagItem outs) <- F.find (named "OutOperandList") (defDecls def)
      Named _ (DagItem ins) <- F.find (named "InOperandList") (defDecls def)
      return (outs, ins, mbits, if endian == Big then toBigEndian mbits else mbits, orderedFieldDefs)
    addFieldName br s =
      case br of
        Just (FieldBit n _) -> S.insert n s
        _ -> s
    isOperandField fieldNames f =
      case f of
        Named n _
          | S.member n fieldNames -> Just n
        _ -> Nothing

-- | Take a list of bits in little endian order and convert it to big endian.
--
-- Group into sub-lists of 8 and then reverse the sub lists.
toBigEndian :: [Maybe BitRef] -> [Maybe BitRef]
toBigEndian = concat . reverse . L.chunksOf 8

-- | Match operands in the operand lists to fields in the instruction based on
-- their variable names.
--
-- This is the most sensible default, but not all ISAs have a clean mapping.
parseOperandsByName :: ISA
                    -> String
                    -- ^ Mnemonic
                    -> [Metadata]
                    -> SimpleValue
                    -> SimpleValue
                    -> [Maybe BitRef]
                    -> (() -> FM [OperandDescriptor])
                    -> FM ([OperandDescriptor], [OperandDescriptor])
parseOperandsByName isa mnemonic (map unMetadata -> metadata) outs ins mbits kexit = do
  outOps <- parseOperandList "outs" outs
  inOps <- parseOperandList "ins" ins
  return (outOps, inOps)
  where
    moverride = lookupFormOverride isa metadata
    -- A map of field names (derived from the 'Inst' field of a
    -- definition) to pairs of (instructionIndex, operandIndex), where
    -- the instruction index is the bit number in the instruction and
    -- the operand index is the index into the operand of that bit.
    operandBits :: M.Map (CI String) [(Int, Int)]
    operandBits = indexFieldBits mbits

    lookupFieldBits :: NL.NonEmpty (String, Int) -> Maybe [(Int, Int)]
    lookupFieldBits (F.toList -> fldNames) = concat <$> mapM lookupAndOffset fldNames
      where
        lookupAndOffset (fldName, offset) = do
          opBits <- M.lookup (CI.mk fldName) operandBits
          return [ (insnIndex, opIndex + offset) | (insnIndex, opIndex) <- opBits ]


    parseOperandList dagHead dagVal =
      case dagVal of
        VDag (DagArg (Identifier hd) _) args
          | hd == dagHead -> F.foldlM parseOperand [] args
        _ -> L.error (printf "Unexpected DAG head (%s) for %s" dagHead mnemonic)

    parseOperand operands arg =
      case arg of
        DagArg (Identifier i) _
          | isaIgnoreOperand isa i -> return operands
          | i == "variable_ops" -> return operands
          | [klass, var] <- L.splitOn ":$" i ->
            case lookupFieldBits (applyOverrides moverride var) of
              Nothing -> do
                -- If we can't do a direct mapping and no override applies, we
                -- have failed and need to record an error for this opcode
                traceM (printf "Failed to find field bits for %s in opcode definition %s" var mnemonic)
                traceM (printf "  overrides were %s" (show (applyOverrides moverride var)))
                St.modify $ \s -> s { stErrors = (mnemonic, var) : stErrors s }
                kexit ()
              Just bitPositions -> do
                let arrVals :: [(Int, Word8)]
                    arrVals = [ (fldIdx, fromIntegral bitNum)
                              | (bitNum, fldIdx) <- bitPositions
                              ]
                    desc = OperandDescriptor { opName = var
                                             , opType = OperandType klass
                                             , opChunks = groupByChunk (L.sortOn snd arrVals)
                                             }
                return (desc : operands)
        _ -> L.error (printf "Unexpected variable reference in a dag for %s: %s" mnemonic (show arg))

indexFieldBits :: [Maybe BitRef] -> M.Map (CI String) [(Int, Int)]
indexFieldBits bits = F.foldl' addBit M.empty (zip [0..] bits)
  where
    addBit m (bitNum, mbit) =
      case mbit of
        Just (FieldBit fldName fldIdx) ->
          M.insertWith (++) (CI.mk fldName) [(bitNum, fldIdx)] m
        _ -> m

applyOverrides :: Maybe FormOverride -> String -> NL.NonEmpty (String, Int)
applyOverrides mOverride varName = fromMaybe ((varName, 0) NL.:| []) $ do
  FormOverride override <- mOverride
  ifd <- lookup varName override
  case ifd of
    SimpleDescriptor var' -> return ((var', 0) NL.:| [])
    ComplexDescriptor chunks -> return chunks

-- | Look up the override associated with the form specified in the metadata, if
-- any.  The first override found is taken, so the ordering matters.
lookupFormOverride :: ISA -> [String] -> Maybe FormOverride
lookupFormOverride isa metadata =
  snd <$> F.find matchOverride (isaFormOverrides isa)
  where
    matchOverride = (`elem` metadata) . fst

-- | Try to make an 'InstructionDescriptor' out of a 'Def'.  May fail
-- (and log its error) or simply skip a 'Def' that fails a test.
toInstructionDescriptor :: ISA -> Def -> FM ()
toInstructionDescriptor isa def = do
  case Metadata "Pseudo" `elem` defMetadata def of
    True -> return ()
    False -> do
      CC.callCC $ \kexit -> do
        withEncoding (isaEndianness isa)  def $ \outs ins mbits endianBits ordFlds -> do
          (outOperands, inOperands) <- parseOperandsByName isa (defName def) (defMetadata def) outs ins mbits kexit
          finishInstructionDescriptor isa def mbits endianBits inOperands outOperands

-- | With the difficult to compute information, finish building the
-- 'InstructionDescriptor' if possible.  If not possible, log an error
-- and continue.
finishInstructionDescriptor :: ISA -> Def -> [Maybe BitRef] -> [Maybe BitRef] -> [OperandDescriptor] -> [OperandDescriptor] -> FM ()
finishInstructionDescriptor isa def mbits endianBits ins outs =
  case mvals of
    Nothing -> return ()
    Just (ns, decoderNs, asmStr, b, cgOnly, asmParseOnly) -> do
      let i = InstructionDescriptor { idMask = map toTrieBit endianBits
                                    , idMaskRaw = map toTrieBit mbits
                                    , idMnemonic = defName def
                                    , idNamespace = ns
                                    , idDecoderNamespace = decoderNs
                                    , idAsmString = asmStr
                                    , idInputOperands = ins
                                    , idOutputOperands = outs
                                    , idPseudo = or [ b -- See Note [Pseudo Instructions]
                                                    , cgOnly
                                                    , asmParseOnly
                                                    , Metadata "Pseudo" `elem` defMetadata def
                                                    ]
                                    }
      when (isaInstructionFilter isa i) $ do
        St.modify $ \s -> s { stInsns = i : stInsns s }
  where
    mvals = do
      Named _ (StringItem ns) <- F.find (named "Namespace") (defDecls def)
      Named _ (StringItem decoderNs) <- F.find (named "DecoderNamespace") (defDecls def)
      Named _ (StringItem asmStr) <- F.find (named "AsmString") (defDecls def)
      Named _ (BitItem b) <- F.find (named "isPseudo") (defDecls def)
      Named _ (BitItem cgOnly) <- F.find (named "isCodeGenOnly") (defDecls def)
      Named _ (BitItem asmParseOnly) <- F.find (named "isAsmParserOnly") (defDecls def)
      return (ns, decoderNs, asmStr, b, cgOnly, asmParseOnly)

-- | Group bits in the operand bit spec into contiguous chunks.
--
-- In most cases, this should be a single chunk.  In rare cases, there will be
-- more.
groupByChunk :: [(Int, Word8)] -> [(Int, Word8, Word8)]
groupByChunk = reverse . map snd . foldr growOrAddChunk []
  where
    -- If the next entry is part of the current chunk, grow the chunk.
    -- Otherwise, begin a new chunk.
    growOrAddChunk (fromIntegral -> operandIndex, fromIntegral -> insnIndex) acc =
      case acc of
        [] -> [(operandIndex, (insnIndex, operandIndex, 1))]
        prev@(lastOpIndex, (chunkInsnIndex, chunkOpIndex, chunkLen)) : rest
          | lastOpIndex == operandIndex + 1 || lastOpIndex == operandIndex - 1 ->
            (operandIndex, (min chunkInsnIndex insnIndex, min chunkOpIndex operandIndex, chunkLen + 1)) : rest
          | otherwise ->
            -- New chunk
            (operandIndex, (insnIndex, operandIndex, 1)) : prev : rest

{- Note [Operand Mapping]

The mapping from operands in the DAG list to fields in the Inst
specification is a bit unclear.  I think it might actually be based on
the ordering of the fields at the bottom of the record (the one that
includes the total number of bits in each field).  It seems like
outputs are mentioned first, then inputs.  Each in the order of the
DAG lists.  That seems consistent across a few instructions and makes
a kind of sense.  That would make the mappings easy to figure out.

-}

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
