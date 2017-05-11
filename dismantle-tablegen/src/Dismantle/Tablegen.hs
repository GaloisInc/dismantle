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
import Data.Tuple (swap)
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NL
import qualified Data.List.Split as L
import qualified Data.Map.Strict as M
import Data.Maybe ( mapMaybe )
import qualified Data.Set as S
import Data.Word ( Word8 )
import Text.Printf ( printf )
import Data.Monoid ((<>))

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

-- | The operand type of the "unpredictable" operand. This is used
-- internally to synthesize an operand referring to any bits in the
-- instruction bit pattern that are also marked as "unpredictable" in
-- the tgen input data. We do this because such bits may not be set to
-- the expected values, but should still be permitted to match in the
-- parser. We also need to parse these bits using an operand so that
-- they are preserved in the reassembled version of such instructions.
unpOperandTy :: String
unpOperandTy = "unpredictable"

-- | The operand name of the "unpredictable" operand.
unpOperandName :: String
unpOperandName = "unpredictable"

-- | Try to extract the basic encoding information from the 'Def'.  If
-- it isn't available, skip the 'Def'.  If it is available, call the
-- continuation that will use it.
withEncoding :: (forall a. [a] -> [a])
             -- ^ The tgen bit pattern transformation function
             -> Def
             -> (SimpleValue -> SimpleValue -> [Maybe BitRef] -> [String] -> FM ())
             -> FM ()
withEncoding tgenBitTransform def k =
  case mvals of
    Just (outs, ins, mbits, ordFlds) -> k outs ins mbits ordFlds
    Nothing -> return ()
  where
    mvals = do
      Named _ (FieldBits rawMbits) <- F.find (named "Inst") (defDecls def)
      -- Inst has all of the names of the fields embedded in the
      -- instruction.  We can collect all of those names into a set.
      -- Then we can filter *all* of the defs according to that set,
      -- maintaining the order of the fields.  We need to return that
      -- here so that we can map fields to DAG items, which tell us
      -- types and direction.
      let mbits = tgenBitTransform rawMbits
          fields = foldr addFieldName S.empty mbits
          orderedFieldDefs = mapMaybe (isOperandField fields) (defDecls def)

      Named _ (DagItem outs) <- F.find (named "OutOperandList") (defDecls def)
      Named _ (DagItem ins) <- F.find (named "InOperandList") (defDecls def)

      -- Does the def provide an Unpredictable bit pattern? If
      -- so, modify the instruction bit pattern so that the bits
      -- corresponding to the Unpredictable bits are associated with
      -- an unpredictable input operand. Then, add that operand to the
      -- input operands list.
      let (ins', mbits') = collectUnpredictableBits tgenBitTransform def ins mbits

      return (outs, ins', mbits', orderedFieldDefs)

    addFieldName br s =
      case br of
        Just (FieldBit n _) -> S.insert n s
        _ -> s

    isOperandField fieldNames f =
      case f of
        Named n _
          | S.member n fieldNames -> Just n
        _ -> Nothing

-- | Given a def and its input operand list and instruction bit
-- pattern, determine whether the def specifies that any of the
-- instruction's bits are Unpredictable. If so, collect those bits
-- into an "Unpredictable" operand, add that operand to the input
-- operand list, and then modify the instruction bit pattern so that
-- unpredictable bits are not fixed but instead refer to the relevant
-- bits in the Unpredictable operand. Return the (potentially) modified
-- input operand list and instruction bit pattern.
--
-- If this function determines that an Unpredictable operand is
-- necessary, the ISA will be required to provide an operand payload
-- type associated with the operand name 'unpOperandName'.
collectUnpredictableBits :: (forall a. [a] -> [a])
                         -- ^ The ISA's bit pattern transformation
                         -- function.
                         -> Def
                         -- ^ The def.
                         -> SimpleValue
                         -- ^ The input operand list.
                         -> [Maybe BitRef]
                         -- ^ The original instruction bit
                         -- pattern, already transformed with
                         -- isaTgenBitPreprocess.
                         -> (SimpleValue, [Maybe BitRef])
collectUnpredictableBits tgenBitTransform def ins mbits =
    case F.find (named "Unpredictable") (defDecls def) of
        Just (Named _ (FieldBits rawUnpBits)) ->
            let unpBits = tgenBitTransform rawUnpBits
                newOperand = DagArg (Identifier (unpOperandTy <> ":$" <> unpOperandName)) Nothing
                newMbits = flip map (zip3 [31,30..0] mbits unpBits) $ \(pos, iBit, unpBit) ->
                    -- If the unpBit is zero, fall back to the iBit pattern entry
                    case unpBit of
                        Just (ExpectedBit True) ->
                            -- Is the instruction bit in this position
                            -- a fixed bit? If so, replace it with an
                            -- operand bit reference.
                            case iBit of
                                Just (ExpectedBit _) -> Just $ FieldBit unpOperandName (OBit pos)
                                Nothing              -> Just $ FieldBit unpOperandName (OBit pos)
                                -- Note that if the instruction bit
                                -- pattern referenced another operand,
                                -- we'll let that original operand
                                -- bit reference stand here by just
                                -- returning the original instruction
                                -- bit entry. This is probably bad
                                -- since such a situation means we have
                                -- real operand bits that are marked as
                                -- Unpredictable. But we do this to be
                                -- conservative and assume that the tgen
                                -- data won't do such a thing.
                                _                    -> iBit
                        _ -> iBit

                newIns = case ins of
                    VDag a as ->
                        -- Only add the operand to the operand list if
                        -- we actually referenced any of its bits in the
                        -- modified bit pattern. Otherwise we'll get an
                        -- unused operand error.
                        let isUnpBit (Just (FieldBit n _))
                              | n == unpOperandName = True
                            isUnpBit _ = False
                        in if any isUnpBit newMbits
                           then VDag a (newOperand:as)
                           else ins
                    _ -> error $ "Unexpected ins: " <> show ins
            in (newIns, newMbits)

        Nothing -> (ins, mbits)
        Just v -> error $ "Unexpected 'Unpredictable' item type: " <> show v

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
    moverride = lookupFormOverride isa mnemonic metadata

    -- A map of field names (derived from the 'Inst' field of a
    -- definition) to pairs of (instructionIndex, operandIndex), where
    -- the instruction index is the bit number in the instruction and
    -- the operand index is the index into the operand of that bit.
    operandBits :: M.Map (CI String) [(IBit, OBit)]
    operandBits = indexFieldBits mbits

    lookupFieldBits :: NL.NonEmpty (String, OBit) -> Maybe [(IBit, OBit)]
    lookupFieldBits (F.toList -> fldNames) = concat <$> mapM lookupAndOffset fldNames
      where
        lookupAndOffset (fldName, offset) = do
          opBits <- M.lookup (CI.mk fldName) operandBits
          return [ (iBit, offset + oBit) | (iBit, oBit) <- opBits ]

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
              case lookupOperandOverride moverride var of
                  Nothing -> return operands
                  Just overrides ->
                      case lookupFieldBits overrides of
                        Nothing -> do
                          -- If we can't do a direct mapping and no override applies, we
                          -- have failed and need to record an error for this opcode
                          traceM (printf "Failed to find field bits for %s in opcode definition %s" var mnemonic)
                          traceM (printf "  overrides were %s" (show overrides))
                          St.modify $ \s -> s { stErrors = (mnemonic, var) : stErrors s }
                          kexit ()
                        Just bitPositions -> do
                          let arrVals :: [(OBit, IBit)]
                              arrVals = swap <$> bitPositions
                              desc = OperandDescriptor { opName = var
                                                       , opType = OperandType klass
                                                       , opChunks = groupByChunk (L.sortOn snd arrVals)
                                                       }
                          return (desc : operands)
        _ -> L.error (printf "Unexpected variable reference in a dag for %s: %s" mnemonic (show arg))

indexFieldBits :: [Maybe BitRef] -> M.Map (CI String) [(IBit, OBit)]
indexFieldBits bits = F.foldl' addBit M.empty (zip bitPositionRange bits)
  where
    bitPositionRange = IBit <$> reverse [0 .. length bits - 1]
    addBit m (iBit, mbit) =
      case mbit of
        Just (FieldBit opFldName opBit) ->
          M.insertWith (++) (CI.mk opFldName) [(iBit, opBit)] m
        _ -> m

-- | Determine whether the named operand decoding behavior needs to be
-- overridden.
--
-- If we were given no overrides at all or if the operand name does not
-- appear at all in a specified override list, assume that the specified
-- operand name has only one chunk in the input word.
--
-- If the operand DOES appear in the override list:
--
-- * If it should be ignored, return Nothing.
-- * If it should be mapped to an alternate name, assume it maps to that
--   name in one chunk.
-- * If it should be mapped to a set of chunks, return Just those
--   chunks.
lookupOperandOverride :: Maybe FormOverride -> String -> Maybe (NL.NonEmpty (String, OBit))
lookupOperandOverride Nothing varName = Just ((varName, OBit 0) NL.:| [])
lookupOperandOverride (Just (FormOverride pairs)) varName =
    case snd <$> filter ((== varName) . fst) pairs of
        [] -> Just ((varName, OBit 0) NL.:| [])
        [e] ->
            case e of
                Ignore                   -> Nothing
                SimpleDescriptor var'    -> Just ((var', OBit 0) NL.:| [])
                ComplexDescriptor chunks -> Just chunks
        _ -> error ""

-- | Look up the override associated with the form specified in the metadata, if
-- any.  The first override found is taken, so the ordering matters.
lookupFormOverride :: ISA -> String -> [String] -> Maybe FormOverride
lookupFormOverride isa mnemonic metadata =
  snd <$> F.find matchOverride (isaFormOverrides isa)
  where
    matchOverride = (\n -> n `elem` metadata || n == mnemonic) . fst

-- | Try to make an 'InstructionDescriptor' out of a 'Def'.  May fail
-- (and log its error) or simply skip a 'Def' that fails a test.
toInstructionDescriptor :: ISA -> Def -> FM ()
toInstructionDescriptor isa def = do
    when (isaInstructionFilter isa def) $ do
      CC.callCC $ \kexit -> do
        withEncoding (isaTgenBitPreprocess isa) def $
          \outs ins mbits _ordFlds -> do
            (outOperands, inOperands) <- parseOperandsByName isa (defName def) (defMetadata def) outs ins mbits kexit
            finishInstructionDescriptor isa def mbits inOperands outOperands

-- | With the difficult to compute information, finish building the
-- 'InstructionDescriptor' if possible.  If not possible, log an error
-- and continue.
finishInstructionDescriptor :: ISA -> Def -> [Maybe BitRef] -> [OperandDescriptor] -> [OperandDescriptor] -> FM ()
finishInstructionDescriptor _isa def mbits ins outs =
  case mvals of
    Nothing -> return ()
    Just (ns, decoderNs, asmStr, b, cgOnly, asmParseOnly) -> do
      let i = InstructionDescriptor { idMask = map toTrieBit mbits
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
      St.modify $ \s -> s { stInsns = i : stInsns s }
  where
    mvals = do
      Named _ (StringItem ns)        <- F.find (named "Namespace") (defDecls def)
      Named _ (StringItem decoderNs) <- F.find (named "DecoderNamespace") (defDecls def)
      Named _ (StringItem asmStr)    <- F.find (named "AsmString") (defDecls def)
      Named _ (BitItem b)            <- F.find (named "isPseudo") (defDecls def)
      Named _ (BitItem cgOnly)       <- F.find (named "isCodeGenOnly") (defDecls def)
      Named _ (BitItem asmParseOnly) <- F.find (named "isAsmParserOnly") (defDecls def)
      return (ns, decoderNs, asmStr, b, cgOnly, asmParseOnly)

-- | Group bits in the operand bit spec into contiguous chunks.
--
-- In most cases, this should be a single chunk.  In rare cases, there will be
-- more.
groupByChunk :: [(OBit, IBit)] -> [(IBit, OBit, Word8)]
groupByChunk = reverse . map snd . foldr growOrAddChunk []
  where
    -- If the next entry is part of the current chunk, grow the chunk.
    -- Otherwise, begin a new chunk.
    growOrAddChunk (oBit, iBit) acc =
      case acc of
        [] -> [(oBit, (iBit, oBit, 1))]
        prev@(lastOpBit, (chunkInsnIndex, chunkOpIndex, chunkLen)) : rest
          | ((lastOpBit == oBit + 1) && (chunkInsnIndex == iBit + 1)) ||
            ((lastOpBit == oBit - 1) && (chunkInsnIndex == iBit - 1)) ->
            (oBit, (min chunkInsnIndex iBit, min chunkOpIndex oBit, chunkLen + 1)) : rest
          | otherwise ->
            -- New chunk
            (oBit, (iBit, oBit, 1)) : prev : rest

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
