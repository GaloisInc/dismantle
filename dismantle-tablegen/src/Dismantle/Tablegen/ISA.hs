{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Dismantle.Tablegen.ISA (
  ISA(..),
  OperandPayload(..),
  FormOverride(..),
  InstFieldDescriptor(..),
  Endianness(..),
  UnusedBitsPolicy(..),
  thumb,
  aarch64,
  mips,
  avr,
  sparc,

  named,
  hasNamedString,
  isPseudo,
  (&&&),
  (|||)
  ) where

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NL
import qualified Text.PrettyPrint.HughesPJClass as PP

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Dismantle.Tablegen.Types
import Dismantle.Tablegen.Parser.Types

data Endianness = Little | Big
  deriving (Eq)

-- | Fragments of AST used during code generation.
--
-- This lets each ISA have its own special types for e.g., registers
-- and immediates.
data OperandPayload =
  OperandPayload { opTypeT :: TypeQ
                   -- ^ The name of the type to use for the payload
                 , opConE :: Maybe ExpQ
                 -- ^ The expression to construct the operand from a Word
                 , opWordE :: Maybe ExpQ
                 -- ^ The expression to turn the operand into the
                 -- instruction's native word type
                 }

-- | Maintains a mapping from operands in the
-- 'OutOperandList'/'InOperandList' to the fields in the 'Inst'
-- definition.
data FormOverride = FormOverride [(String, InstFieldDescriptor)]
                  deriving (Show)

data InstFieldDescriptor = SimpleDescriptor String
                         -- ^ A simple mapping to another string
                         | ComplexDescriptor (NL.NonEmpty (String, OBit))
                         -- ^ A mapping to a set of strings that represent
                         -- chunks of the operand
                         | Ignore
                         -- ^ The operand with this descriptor should be
                         -- ignored (e.g. for operands that appear in
                         -- the operand lists but appear nowhere in the
                         -- bit specfications).
                         deriving (Show, Eq)

-- | How to handle instruction bit patterns that provide more bits
-- than the instruction claims to need. Specifically, an instruction
-- description might provide a pattern with 32 bits but only use the
-- rightmost 16 bits. In that case a policy of Drop, meaning to drop
-- the first 16 bits, would be appropriate. If, on the other hand,
-- the instruction specified 32 bits but only used the leftmost 16, a
-- policy of Take would be the right choice. The choice depends on the
-- conventions used in the Tablegen data for the ISA in question.
data UnusedBitsPolicy = Take | Drop

-- | Information specific to an ISA that influences code generation
data ISA =
  ISA { isaName :: String
      , isaTgenBitPreprocess :: forall a. [a] -> [a]
      -- ^ A function to preprocess the bit patterns found in the Tgen
      -- data. This function is responsible for transforming "Inst" bit
      -- pattern lists from the Tgen data so that they are ordered with
      -- the most significant bit first in the list.
      , isaInputEndianness :: Endianness
      -- ^ The endianness of the input bytes when parsing an instruction
      -- stream
      , isaUnusedBitsPolicy :: Maybe UnusedBitsPolicy
      -- ^ How to handle instructions that specify longer bit patterns
      -- than they actually need
      , isaInstructionFilter :: Def -> Bool
      -- ^ A function that should return True for the def if it is part
      -- of the ISA and False if not.
      , isaPseudoInstruction :: InstructionDescriptor -> Bool
      -- ^ Return 'True' if the instruction is a pseudo-instruction
      -- that should not be disassembled.  As an example, some
      -- instructions have identical representations at the machine
      -- code level, but have special interpretation of e.g.,
      -- constants at assembly time.  We can only disassemble to one,
      -- so this function should mark the non-canonical versions of
      -- instructions as pseudo.
      --
      -- Note, we might need more information here later to let us
      -- provide pretty disassembly of instructions
      , isaOperandPayloadTypes :: [(String, OperandPayload)]
      -- ^ Per-ISA operand customization.  This lets us have a custom
      -- register type for each ISA, for example.
      , isaIgnoreOperand :: String -> Bool
      -- ^ Operands in the DAG argument lists that should be ignored for this
      -- ISA.  There are some operands that are not actually encoded in the
      -- instruction, and seem to only serve to carry metadata.  An example from
      -- PPC is @ptr_rc_nor0:$ea_res@, which is just a marker to indicate that
      -- the instruction updates memory at a location held in a register.
      , isaFormOverrides :: [(String, FormOverride)]
      -- ^ A list of *ordered* overrides to apply to operand mappings based on
      -- the forms specified in instruction metadata.  The first match will be
      -- used as the override (if any).
      , isaPrettyOverrides :: [(String, [(String, String)])]
      -- ^ A mapping of per-operand pretty-printing overrides. The keys
      -- in the map are tablegen def names (same as in isaFormOverrides)
      -- and the values are lists that map operand names to their
      -- pretty-printing representations. Mappings provided here take
      -- precedence over the defaults in isaDefaultPrettyVariableValues.
      , isaInsnWordFromBytes :: Name
      -- ^ The name of the function that is used to convert a prefix
      -- of the instruction stream into a single word that contains an
      -- instruction (e.g., ByteString -> Word32)
      , isaInsnAssembleType :: Name
      -- ^ The word type to use to assemble instructions into bytes
      , isaInsnWordToBytes :: Name
      -- ^ The function to use to turn the assembly word (e.g., a
      -- Word32) into a bytestring.
      , isaMapOperandPayloadType :: String -> String
      -- ^ Convert from one operand payload type to another; this is useful to
      -- let us treat operand types that tablegen treats as distinct uniformly.
      -- We use this for PowerPC to treat the 'g8rc' register type as 'gprc'.
      , isaDefaultPrettyVariableValues :: [(String, String)]
      -- ^ Default representations for variables encountered in
      -- instruction descriptor format strings when those variables
      -- are not mentioned elsewhere in the descriptor and thus have
      -- no value. Some descriptors may reference variables in format
      -- strings even if those variables are not defined as input or
      -- output operands or are unmentioned in the instruction's bit
      -- pattern.
      }

thumb :: ISA
thumb = ISA { isaName = "Thumb"
            , isaInputEndianness = Little
            , isaTgenBitPreprocess = id
            , isaInstructionFilter = thumbFilter
            , isaPseudoInstruction = const False
            , isaUnusedBitsPolicy = Just Drop
            }
  where
    thumbFilter = hasNamedString "DecoderNamespace" "Thumb" &&&
                  hasNamedString "Namespace" "ARM" &&&
                  (not . isPseudo)

aarch64 :: ISA
aarch64 = ISA { isaName = "AArch64"
              , isaInputEndianness = Big
              , isaTgenBitPreprocess = id
              , isaInstructionFilter = aarch64Filter
              , isaPseudoInstruction = const False
              , isaUnusedBitsPolicy = Nothing
              }
  where
    aarch64Filter = hasNamedString "Namespace" "AArch64" &&&
                    (not . isPseudo)

unadorned :: Type -> BangType
unadorned t = (Bang NoSourceUnpackedness NoSourceStrictness, t)

mips :: ISA
mips = ISA { isaName = "Mips"
           , isaInputEndianness = Big
           , isaTgenBitPreprocess = id
           , isaInstructionFilter = mipsFilter
           , isaPseudoInstruction = const False
           , isaUnusedBitsPolicy = Nothing
           }
  where
    mipsFilter = hasNamedString "DecoderNamespace" "Mips" &&&
                 hasNamedString "Namespace" "Mips" &&&
                 (not . isPseudo)

avr :: ISA
avr = ISA { isaName = "AVR"
          , isaTgenBitPreprocess = id
          , isaInstructionFilter = avrFilter
          , isaPseudoInstruction = avrPsuedo
          , isaUnusedBitsPolicy = Nothing
          }
  where
    avrFilter = hasNamedString "Namespace" "AVR"
    avrPsuedo i = idPseudo i ||
                  idMnemonic i `elem` [ "CBRRdK" -- Clear bits, equivalent to an ANDi
                                      , "LSLRd"  -- Equivalent to add rd, rd
                                      , "ROLRd"
                                      , "SBRRdK" -- Equivalent to ORi
                                      , "TSTRd"  -- Equivalent to AND Rd,Rd
                                      , "LDDRdPtrQ" -- Sometimes an alias of LDRdPtr, but not always...
                                      , "BRLOk" -- brbs 0,k
                                      , "BRLTk" -- brbs 4,k
                                      , "BRMIk" -- brbs 2,k
                                      , "BREQk" -- brbs 1,k
                                      , "BRSHk" -- brbc 0,k
                                      , "BRGEk" -- brbc 4,k
                                      , "BRPLk" -- brbc 2,k
                                      , "BRNEk" -- brbc 1,k
                                      , "STDPtrQRr" -- similar to the LDDRdPtrQ above
                                      ]

sparc :: ISA
sparc = ISA { isaName = "Sparc"
            , isaInputEndianness = Big
            , isaTgenBitPreprocess = id
            , isaInstructionFilter = sparcFilter
            , isaPseudoInstruction = const False
            , isaUnusedBitsPolicy = Nothing
            }
  where
    sparcFilter = hasNamedString "Namespace" "SP" &&&
                  hasNamedString "DecoderNamespace" "Sparc" &&&
                  (not . isPseudo)

isPseudo :: Def -> Bool
isPseudo def =
    let binding = case F.find (named "isPseudo") (defDecls def) of
            Just (Named _ (BitItem b)) -> b
            _ -> False
        metadata = Metadata "Pseudo" `elem` (defMetadata def)
    in binding || metadata

hasNamedString :: String -> String -> Def -> Bool
hasNamedString label value def =
    case F.find (named label) (defDecls def) of
        Just (Named _ (StringItem s)) -> s == value
        _ -> False

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) f g val = f val && g val

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) f g val = f val || g val

named :: String -> Named DeclItem -> Bool
named s n = namedName n == s

{-

Note, each ISA has a large list of "ignorable" instructions.  We could
automatically ignore the specialized instances and record the parse as
the *most general* (i.e., the one with the fewest Expected Bits).

We could then maintain the mapping of those instructions so we could
automatically render specialized versions in the pretty printer...

The disadvantage is that mistakes could go unnoticed more easily.

-}
