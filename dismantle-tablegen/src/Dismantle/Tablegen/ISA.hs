{-# LANGUAGE TemplateHaskell #-}
module Dismantle.Tablegen.ISA (
  ISA(..),
  OperandPayload(..),
  FormOverride(..),
  InstFieldDescriptor(..),
  Endianness(..),
  thumb,
  aarch64,
  mips,
  avr,
  sparc,

  named,
  hasNamedString,
  isPseudo,
  (&&&)
  ) where

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NL

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
  OperandPayload { opTypeName :: Name
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
                         | ComplexDescriptor (NL.NonEmpty (String, Int))
                         -- ^ A mapping to a set of strings that represent
                         -- chunks of the operand
                         | Ignore
                         -- ^ The operand with this descriptor should be
                         -- ignored (e.g. for operands that appear in
                         -- the operand lists but appear nowhere in the
                         -- bit specfications).
                         deriving (Show, Eq)

-- | Information specific to an ISA that influences code generation
data ISA =
  ISA { isaName :: String
      , isaEndianness :: Endianness
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
      , isaInsnWordFromBytes :: Name
      -- ^ The name of the function that is used to convert a prefix
      -- of the instruction stream into a single word that contains an
      -- instruction (e.g., ByteString -> Word32)
      , isaInsnAssembleType :: Name
      -- ^ The word type to use to assemble instructions into bytes
      , isaInsnWordToBytes :: Name
      -- ^ The function to use to turn the assembly word (e.g., a
      -- Word32) into a bytestring.
      }


thumb :: ISA
thumb = ISA { isaName = "Thumb"
            , isaEndianness = Little
            , isaInstructionFilter = thumbFilter
            , isaPseudoInstruction = const False
            }
  where
    thumbFilter = hasNamedString "DecoderNamespace" "Thumb" &&&
                  hasNamedString "Namespace" "ARM" &&&
                  (not . isPseudo)

aarch64 :: ISA
aarch64 = ISA { isaName = "AArch64"
              , isaEndianness = Little
              , isaInstructionFilter = aarch64Filter
              , isaPseudoInstruction = const False
              }
  where
    aarch64Filter = hasNamedString "Namespace" "AArch64" &&&
                    (not . isPseudo)

unadorned :: Type -> BangType
unadorned t = (Bang NoSourceUnpackedness NoSourceStrictness, t)

mips :: ISA
mips = ISA { isaName = "Mips"
           , isaEndianness = Big
           , isaInstructionFilter = mipsFilter
           , isaPseudoInstruction = const False
           }
  where
    mipsFilter = hasNamedString "DecoderNamespace" "Mips" &&&
                 hasNamedString "Namespace" "Mips" &&&
                 (not . isPseudo)

avr :: ISA
avr = ISA { isaName = "AVR"
          , isaInstructionFilter = avrFilter
          , isaPseudoInstruction = avrPsuedo
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
            , isaEndianness = Big
            , isaInstructionFilter = sparcFilter
            , isaPseudoInstruction = const False
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
