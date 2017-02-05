{-# LANGUAGE TemplateHaskell #-}
module Dismantle.Tablegen.ISA (
  ISA(..),
  OperandPayload(..),
  Endianness(..),
  arm,
  thumb,
  aarch64,
  ppc,
  mips,
  avr,
  sparc
  ) where

import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import qualified Data.List as L
import Data.Word
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Dismantle.PPC.Operands as PPC
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
      }

asWord32 :: LBS.ByteString -> Word32
asWord32 = B.runGet B.getWord32be

arm :: ISA
arm = ISA { isaName = "ARM"
          , isaEndianness = Little
          , isaInstructionFilter = armFilter
          , isaPseudoInstruction = const False
          }
  where
    armFilter i = idDecoder i == "ARM" && idNamespace i == "ARM" && not (idPseudo i)

thumb :: ISA
thumb = ISA { isaName = "Thumb"
            , isaEndianness = Little
            , isaInstructionFilter = thumbFilter
            , isaPseudoInstruction = const False
            }
  where
    thumbFilter i = idDecoder i == "Thumb" && idNamespace i == "ARM" && not (idPseudo i)

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

ppc :: ISA
ppc = ISA { isaName = "PPC"
          , isaEndianness = Big
          , isaInstructionFilter = ppcFilter
          , isaPseudoInstruction = ppcPseudo
          , isaOperandPayloadTypes = ppcOperandPayloadTypes
          , isaInsnWordFromBytes = 'asWord32
          }
  where
    absoluteAddress = OperandPayload { opTypeName = ''Word64
                                     , opConName = Nothing
                                     }
    relativeOffset = OperandPayload { opTypeName = ''Int64
                                    , opConName = Nothing
                                    }
    gpRegister = OperandPayload { opTypeName = ''PPC.GPR
                                , opConName = Just 'PPC.GPR
                                }
    conditionRegister = OperandPayload { opTypeName = ''PPC.CR
                                       , opConName = Just 'PPC.CR
                                       }
    floatRegister = OperandPayload { opTypeName = ''PPC.FR
                                   , opConName = Just 'PPC.FR
                                   }
    signedImmediate :: Word8 -> OperandPayload
    signedImmediate _n = OperandPayload { opTypeName = ''Int64
                                        , opConName = Nothing
                                        }
    unsignedImmediate :: Word8 -> OperandPayload
    unsignedImmediate _n = OperandPayload { opTypeName = ''Word64
                                          , opConName = Nothing
                                          }
    vecRegister = OperandPayload { opTypeName = ''PPC.VR
                                 , opConName = Just 'PPC.VR
                                 }
    ppcOperandPayloadTypes =
      [ ("Abscondbrtarget", absoluteAddress)
      , ("Absdirectbrtarget", absoluteAddress)
      , ("Condbrtarget", relativeOffset)
      , ("Directbrtarget", absoluteAddress)
      , ("Calltarget", relativeOffset)
      , ("Abscalltarget", absoluteAddress)
      , ("Ptr_rc_nor0", gpRegister) -- fixme
      , ("Tlscall", gpRegister) -- fixme
      , ("Tlscall32", gpRegister) --fixme
      , ("Spe8dis", gpRegister) -- fixme
      , ("Spe2dis", gpRegister)
      , ("Spe4dis", gpRegister)
      , ("Crbitm", conditionRegister)  -- these two are very odd, must investigate
      , ("Crbitrc", conditionRegister)
      , ("Crrc", conditionRegister) -- 4 bit
      , ("F4rc", floatRegister)
      , ("F8rc", floatRegister)
      , ("G8rc", gpRegister)
      , ("Gprc", gpRegister)
        -- These two variants are special for instructions that treat r0 specially
      , ("Gprc_nor0", gpRegister)
      , ("G8rc_nox0", gpRegister)
      , ("I1imm", signedImmediate 1)
      , ("I32imm", signedImmediate 32)
      , ("S16imm", signedImmediate 16)
      , ("S16imm64", signedImmediate 16)
      , ("S17imm", signedImmediate 17)
      , ("S17imm64", signedImmediate 17)
      , ("S5imm", signedImmediate 5)
      , ("Tlsreg", gpRegister)
      , ("Tlsreg32", gpRegister)
      , ("U1imm", unsignedImmediate 1)
      , ("U2imm", unsignedImmediate 2)
      , ("U4imm", unsignedImmediate 4)
      , ("U5imm", unsignedImmediate 5)
      , ("U6imm", unsignedImmediate 6)
      , ("U7imm", unsignedImmediate 7)
      , ("U8imm", unsignedImmediate 8)
      , ("U16imm", unsignedImmediate 16)
      , ("U16imm64", unsignedImmediate 16)
      , ("Memrr", gpRegister) -- FIXME:
      , ("Memri", gpRegister) -- FIXME: these are mem refs
      , ("Memrix", gpRegister)
      , ("Memrix16", gpRegister)
      , ("Pred", gpRegister)
      , ("Vrrc", vecRegister)
      , ("Vsfrc", vecRegister) -- floating point vec?
      , ("Vsrc", vecRegister) -- ??
      , ("Vssrc", vecRegister) -- ??
      ]

    ppcFilter i = and [ idNamespace i == "PPC"
                      , idDecoder i == ""
                      , L.last (idMnemonic i) /= '8'
                      -- FIXME: What is going on here? The operands don't make sense
                      , idMnemonic i `notElem` [ "XSRQPXP"
                                               , "XSRQPIX"
                                               , "XSRQPI"
                                               , "XORIS8"
                                               , "XORIS"
                                               , "XORI8"
                                               , "XORI"
                                               , "UpdateGBR"
                                               , "UPDATE_VRSAVE"
                                               , "TWI"
                                               , "TW"
                                               , "TSR"
                                               , "TRECLAIM"
                                               , "TRECHKPT"
                                               , "TLBWE2"
                                               , "TLBRE2"
                                               , "TLBLI"
                                               , "TLBLD"
                                               , "TLBIEL"
                                               , "TLBIE"
                                               , "TEND"
                                               , "TDI"
                                               , "TCRETURNri8"
                                               , "TCRETURNri"
                                               , "TCRETURNdi8"
                                               , "TCRETURNdi"
                                               , "TCRETURNai8"
                                               , "TCRETURNai"
                                               , "TCHECK_RET"
                                               , "TBEGIN"
                                               , "TAILBA8"
                                               , "TAILBA"
                                               , "TAILB8"
                                               , "TAILB"
                                               , "TABORTWCI"
                                               , "TABORTWC"
                                               , "TABORTDCI"
                                               , "TABORTDC"
                                               , "TABORT"
                                               , "STXVX" -- has one field in DAG, but two in bit pattern
                                               , "STXVW4X"
                                               ]
                      ]
    ppcPseudo i = idPseudo i ||
                  idMnemonic i `elem` [ "LI" -- li rD,val == addi rD,0,val
                                      , "LIS" -- ~same
                                      , "BDNZ" -- subsumed by gBC... maybe just BC?
                                      , "BDNZm"
                                      , "BDNZp"
                                      , "BDZ"
                                      , "BDZm"
                                      , "BDZp"
                                      , "BDZL"
                                      , "BDZLm"
                                      , "BDZLp"
                                      , "BDZA"
                                      , "BDZAm"
                                      , "BDZAp"
                                      , "BDNZLA"
                                      , "BDZLA"
                                      , "BDZLAm"
                                      , "BDZLAp"
                                      , "BDNZLAm"
                                      , "BDNZLAp"
                                      , "BDNZA"
                                      , "BDNZAm"
                                      , "BDNZAp"
                                      , "BDNZL"
                                      , "BDNZLm"
                                      , "BDNZLp"
                                      , "BDNZLR"
                                      , "BDNZLRm"
                                      , "BDNZLRp"
                                      , "BDZLR"
                                      , "BDZLRm"
                                      , "BDZLRp"
                                      , "BLR"
                                      , "BLRm"
                                      , "BLRp"
                                      , "BDNZLRL"
                                      , "BDNZLRLm"
                                      , "BDNZLRLp"
                                      , "BDZLRL"
                                      , "BDZLRLm"
                                      , "BDZLRLp"
                                      , "BLRL"
                                      , "BLRLm"
                                      , "BLRLp"
                                      , "BCTR"
                                      , "BCTRL"
                                      , "TLBSX2"
                                      , "TLBRE2"
                                      , "TLBWE2"
                                      , "TLBLD"
                                      , "MFLR"
                                      , "MFXER"
                                      , "MFCTR"
                                      , "MTLR"
                                      , "MTXER"
                                      , "MTCTR"
                                      , "EnforceIEIO"
                                      , "NOP" -- encoded as OR r, 0?  maybe even or r0 r0
                                      , "TRAP" -- encoded as TW (trap word) some constant
                                      ]

mips :: ISA
mips = ISA { isaName = "Mips"
           , isaEndianness = Big
           , isaInstructionFilter = mipsFilter
           , isaPseudoInstruction = const False
           }
  where
    mipsFilter i = idDecoder i == "Mips" && idNamespace i == "Mips" && not (idPseudo i)

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
    sparcFilter i = idNamespace i == "SP" && idDecoder i == "Sparc" && not (idPseudo i)

{-

Note, each ISA has a large list of "ignorable" instructions.  We could
automatically ignore the specialized instances and record the parse as
the *most general* (i.e., the one with the fewest Expected Bits).

We could then maintain the mapping of those instructions so we could
automatically render specialized versions in the pretty printer...

The disadvantage is that mistakes could go unnoticed more easily.

-}
