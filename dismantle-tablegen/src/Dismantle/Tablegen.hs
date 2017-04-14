{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
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

import Control.Monad ( guard, when, unless )
import qualified Control.Monad.Cont as CC
import qualified Control.Monad.State.Strict as St
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map.Strict as M
import Data.Maybe ( mapMaybe )
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

-- | Try to make an 'InstructionDescriptor' out of a 'Def'.  May fail
-- (and log its error) or simply skip a 'Def' that fails a test.
toInstructionDescriptor :: ISA -> Def -> FM ()
toInstructionDescriptor isa def = do
  CC.callCC $ \kexit -> do
    withEncoding (isaEndianness isa)  def $ \outs ins mbits endianBits ordFlds -> do
      -- Change mkOperandDescriptors such that it takes ordFlds and
      -- returns the set that have not been consumed.  Note that the
      -- outputs are listed first, so we have to process outputs first.
      --
      -- The current processing is only valid if fields can only be
      -- outputs OR inputs (not both).  Note that this is distinct
      -- from the same register being encoded in two slots - it refers
      -- to x86-style instructions where e.g. EAX is both read as an
      -- input and modified as a side effect of an instruction, while
      -- only being encoded in the instruction stream once.
      (outOperands, ordFlds') <- mkOperandDescriptors (defName def) "outs" outs ordFlds mbits kexit
      (inOperands, ordFlds'') <- mkOperandDescriptors (defName def) "ins" ins ordFlds' mbits kexit
      unless (null ordFlds'') $ do
        St.modify $ \s -> s { stErrors = (defName def, "") : stErrors s }
      finishInstructionDescriptor isa def mbits endianBits inOperands outOperands

-- | With the difficult to compute information, finish building the
-- 'InstructionDescriptor' if possible.  If not possible, log an error
-- and continue.
finishInstructionDescriptor :: ISA -> Def -> [Maybe BitRef] -> [Maybe BitRef] -> [OperandDescriptor] -> [OperandDescriptor] -> FM ()
finishInstructionDescriptor isa def mbits endianBits ins outs =
  case mvals of
    Nothing -> return ()
    Just (ns, decoder, asmStr, b, cgOnly, asmParseOnly) -> do
      let i = InstructionDescriptor { idMask = map toTrieBit endianBits
                                    , idMaskRaw = map toTrieBit mbits
                                    , idMnemonic = defName def
                                    , idNamespace = ns
                                    , idDecoder = decoder
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
      Named _ (StringItem decoder) <- F.find (named "DecoderNamespace") (defDecls def)
      Named _ (StringItem asmStr) <- F.find (named "AsmString") (defDecls def)
      Named _ (BitItem b) <- F.find (named "isPseudo") (defDecls def)
      Named _ (BitItem cgOnly) <- F.find (named "isCodeGenOnly") (defDecls def)
      Named _ (BitItem asmParseOnly) <- F.find (named "isAsmParserOnly") (defDecls def)
      return (ns, decoder, asmStr, b, cgOnly, asmParseOnly)

-- | Extract OperandDescriptors from the bits pattern, given type
-- information from the InOperandList or OutOperandList DAG items.
--
-- The important thing is that we are preserving the *order* of
-- operands here so that users have a chance of making sense of the
-- generated instructions.
mkOperandDescriptors :: String
                     -- ^ Instruction mnemonic
                     -> String
                     -- ^ DAG operator string ("ins" or "outs")
                     -> SimpleValue
                     -- ^ The DAG item (either InOperandList or OutOperandList)
                     -> [String]
                     -- ^ Field name order
                     -> [Maybe BitRef]
                     -- ^ The bits descriptor so that we can find the bit numbers for the field
                     -> (() -> FM ([OperandDescriptor], [String]))
                     -- ^ Early termination continuation
                     -> FM ([OperandDescriptor], [String])
mkOperandDescriptors mnemonic dagOperator dagItem ordFlds bits kexit =
  -- Leave this case as an error, since that means we have a tablegen
  -- structure we didn't expect rather than a fixable data mismatch
  case dagItem of
    VDag (DagArg (Identifier hd) _) args
      | hd == dagOperator -> F.foldlM parseOperand ([], ordFlds) args
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

    parseOperand :: ([OperandDescriptor], [String]) -> DagArg -> FM ([OperandDescriptor], [String])
    parseOperand acc@(ops, fldNames) arg =
      case arg of
        DagArg (Identifier i) _
          | [klass, var] <- L.splitOn ":$" i ->
            case fldNames of
              [] -> do
                St.modify $ \s -> s { stErrors = (mnemonic, var) : stErrors s }
                kexit ()
              fldName : rest ->
                case lookupFieldBits fldName of
                  Nothing -> do
                    St.modify $ \s -> s { stErrors = (mnemonic, var) : stErrors s }
                    kexit ()
                  Just bitPositions -> do
                    let arrVals = [ (fldIdx, fromIntegral bitNum)
                              | (bitNum, fldIdx) <- bitPositions
                              ]
                        bitPositions = L.sortOn fst arrVals
                        desc = OperandDescriptor { opName = var
                                                    , opType = OperandType klass
                                                    , opBits = bitPositions
                                                    , opChunks = groupByChunk bitPositions
                                                    , opStartBit = fromIntegral (minimum (map snd arrVals))
                                                    , opNumBits = length arrVals
                                                    }
                    return (desc : ops, rest)
          | i == "variable_ops" ->
            -- This case is expected sometimes - there is no single
            -- argument to make (yet, we might add one)
            return acc
        _ -> L.error ("Unexpected variable reference in a DAG for " ++ mnemonic ++ ": " ++ show arg)

    lookupFieldBits :: String -> Maybe [(Int, Int)]
    lookupFieldBits fldName = M.lookup (CI.mk fldName) operandBits

-- | Group bits in the operand bit spec into contiguous chunks.
--
-- In most cases, this should be a single chunk.  In rare cases, there will be
-- more.
--
-- FIXME: This needs to account for endianness in the next/last bit test
groupByChunk :: [(Int, Word8)] -> [(Int, Word8, Word8)]
groupByChunk = reverse . foldr growOrAddChunk []
  where
    -- If the next entry is part of the current chunk, grow the chunk.
    -- Otherwise, begin a new chunk.
    growOrAddChunk (insnIndex, operandIndex) acc =
      case acc of
        [] -> [(operandIndex, (insnIndex, operandIndex, 1))]
        (lastOpIndex, (chunkInsnIndex, chunkOpIndex, chunkLen)) : rest
          | lastOpIndex /= operandIndex + 1 ->
            -- New chunk
            undefined
          | otherwise -> (operandIndex, (chunkInsnIndex, chunkOpIndex, chunkLen + 1)) : rest

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
