{-# LANGUAGE TemplateHaskell #-}
module Dismantle.Tablegen.ISA (
  ISA(..),
  OperandPayload(..),
  Endianness(..),
  thumb,
  aarch64,
  mips,
  avr,
  sparc
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Dismantle.Tablegen.Types

data Endianness = Little | Big
  deriving (Eq)

-- | Fragments of AST used during code generation.
--
-- This lets each ISA have its own special types for e.g., registers
-- and immediates.
data OperandPayload =
  OperandPayload { opConName :: Maybe Name
                   -- ^ The type constructor to wrap around the actual
                   -- payload data.  If there is none, don't apply a
                   -- wrapper.
                 , opTypeName :: Name
                   -- ^ The name of the type to use for the payload
                 , opConE :: Maybe ExpQ
                 -- ^ The expression to construct the operand from a Word
                 , opWordE :: Maybe ExpQ
                 -- ^ The expression to turn the operand into the
                 -- instruction's native word type
                 }

-- | Information specific to an ISA that influences code generation
data ISA =
  ISA { isaName :: String
      , isaEndianness :: Endianness
      , isaInstructionFilter :: InstructionDescriptor -> Bool
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
    thumbFilter i = idDecoderNamespace i == "Thumb" && idNamespace i == "ARM" && not (idPseudo i)

aarch64 :: ISA
aarch64 = ISA { isaName = "AArch64"
              , isaEndianness = Little
              , isaInstructionFilter = aarch64Filter
              , isaPseudoInstruction = const False
              }
  where
    aarch64Filter i = idNamespace i == "AArch64" && not (idPseudo i)

unadorned :: Type -> BangType
unadorned t = (Bang NoSourceUnpackedness NoSourceStrictness, t)

mips :: ISA
mips = ISA { isaName = "Mips"
           , isaEndianness = Big
           , isaInstructionFilter = mipsFilter
           , isaPseudoInstruction = const False
           }
  where
    mipsFilter i = idDecoderNamespace i == "Mips" && idNamespace i == "Mips" && not (idPseudo i)

avr :: ISA
avr = ISA { isaName = "AVR"
          , isaInstructionFilter = avrFilter
          , isaPseudoInstruction = avrPsuedo
          }
  where
    avrFilter i = idNamespace i == "AVR"
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
    sparcFilter i = idNamespace i == "SP" && idDecoderNamespace i == "Sparc" && not (idPseudo i)

{-

Note, each ISA has a large list of "ignorable" instructions.  We could
automatically ignore the specialized instances and record the parse as
the *most general* (i.e., the one with the fewest Expected Bits).

We could then maintain the mapping of those instructions so we could
automatically render specialized versions in the pretty printer...

The disadvantage is that mistakes could go unnoticed more easily.

-}
