{-# LANGUAGE TemplateHaskell #-}
module Dismantle.Tablegen.ISA (
  ISA(..),
  OperandPayload(..),
  arm,
  thumb,
  aarch64,
  ppc,
  mips,
  avr,
  sparc
  ) where

import Data.Int
import Data.Word
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Dismantle.PPC.Operands as PPC
import Dismantle.Tablegen.Types

-- | Fragments of AST used during code generation.
--
-- This lets each ISA have its own special types for e.g., registers
-- and immediates.
data OperandPayload =
  OperandPayload { opTypeDecls :: [Dec]
                   -- ^ This can be empty if there is no additional
                   -- wrapper type (i.e., we represent it as an inline
                   -- primitive type)
                 , opConName :: Maybe Name
                   -- ^ The type constructor to wrap around the actual
                   -- payload data.  If there is none, don't apply a
                   -- wrapper.
                 , opTypeName :: Name
                   -- ^ The name of the type to use for the payload
                 }

-- | Information specific to an ISA that influences code generation
data ISA =
  ISA { isaName :: String
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
      , isaOperandClassMapping :: String -> [String]
      -- ^ In the tablegen files, sometimes the operand names
      -- referenced in the input and output lists does not match the
      -- names in the bit fields of the 'Instruction'.  This function
      -- lets us specify a mapping between the broken pairs.
      , isaOperandPayloadTypes :: [(String, OperandPayload)]
      -- ^ Per-ISA operand customization.  This lets us have a custom
      -- register type for each ISA, for example.
      }

arm :: ISA
arm = ISA { isaName = "ARM"
          , isaInstructionFilter = armFilter
          , isaPseudoInstruction = const False
          , isaOperandClassMapping = const []
          }
  where
    armFilter i = idDecoder i == "ARM" && idNamespace i == "ARM" && not (idPseudo i)

thumb :: ISA
thumb = ISA { isaName = "Thumb"
            , isaInstructionFilter = thumbFilter
            , isaPseudoInstruction = const False
            , isaOperandClassMapping = const []
            }
  where
    thumbFilter i = idDecoder i == "Thumb" && idNamespace i == "ARM" && not (idPseudo i)

aarch64 :: ISA
aarch64 = ISA { isaName = "AArch64"
              , isaInstructionFilter = aarch64Filter
              , isaPseudoInstruction = const False
              , isaOperandClassMapping = const []
              }
  where
    aarch64Filter i = idNamespace i == "AArch64" && not (idPseudo i)

unadorned :: Type -> BangType
unadorned t = (Bang NoSourceUnpackedness NoSourceStrictness, t)

ppc :: ISA
ppc = ISA { isaName = "PPC"
          , isaInstructionFilter = ppcFilter
          , isaPseudoInstruction = ppcPseudo
          , isaOperandClassMapping = ppcOperandMapping
          , isaOperandPayloadTypes = ppcOperandPayloadTypes
          }
  where
    absoluteAddress = OperandPayload { opTypeName = ''Word64
                                     , opConName = Nothing
                                     , opTypeDecls = []
                                     }
    relativeOffset = OperandPayload { opTypeName = ''Int64
                                    , opConName = Nothing
                                    , opTypeDecls = []
                                    }
    gpRegister = OperandPayload { opTypeName = ''PPC.GPR
                                , opConName = Just 'PPC.GPR
                                , opTypeDecls = []
                                }
    conditionRegister = OperandPayload { opTypeName = ''PPC.CR
                                       , opConName = Just 'PPC.CR
                                       , opTypeDecls = []
                                       }
    floatRegister = OperandPayload { opTypeName = ''PPC.FR
                                   , opConName = Just 'PPC.FR
                                   , opTypeDecls = []
                                   }
    signedImmediate :: Word8 -> OperandPayload
    signedImmediate _n = OperandPayload { opTypeName = ''Int64
                                        , opConName = Nothing
                                        , opTypeDecls = []
                                        }
    unsignedImmediate :: Word8 -> OperandPayload
    unsignedImmediate _n = OperandPayload { opTypeName = ''Word64
                                          , opConName = Nothing
                                          , opTypeDecls = []
                                          }
    vecRegister = OperandPayload { opTypeName = ''PPC.VR
                                 , opConName = Just 'PPC.VR
                                 , opTypeDecls = []
                                 }
    ppcOperandPayloadTypes =
      [ ("Abscondbrtarget", absoluteAddress)
      , ("Condbrtarget", relativeOffset)
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
      , ("Vrrc", vecRegister)
      , ("Vsfrc", vecRegister) -- floating point vec?
      , ("Vsrc", vecRegister) -- ??
      , ("Vssrc", vecRegister) -- ??
      ]

    ppcOperandMapping i =
      case i of
        "dst" -> ["BD"]
        "UIM" -> ["D", "UIM5"]
        "UIMM" -> ["UIM5", "VA"]
        "SIMM" -> ["IMM"]
        "imm" -> ["C"]
        "crD" -> ["CR", "BF"]
        "vB" -> ["B", "FRB"]
        "VB" -> ["B", "FRB"]
        "vT" -> ["RST", "FRT"]
        "vA" -> ["A", "FRA"]
        "VA" -> ["A", "FRA"]
        "vD" -> ["RD"]
        "SHW" -> ["D"]
        "DM" -> ["D"]
        "XTi" -> ["XT"]
        "rmc" -> ["idx"]
        "rA" -> ["A", "VA"]
        "rB" -> ["B", "VB"]
        "rD" -> ["VD", "B"]
        "rS" -> ["RST"]
        "to" -> ["RST"]
        _ -> []
    ppcFilter i = and [ idNamespace i == "PPC"
                      , idDecoder i == ""
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
           , isaInstructionFilter = mipsFilter
           , isaPseudoInstruction = const False
           , isaOperandClassMapping = const []
           }
  where
    mipsFilter i = idDecoder i == "Mips" && idNamespace i == "Mips" && not (idPseudo i)

avr :: ISA
avr = ISA { isaName = "AVR"
          , isaInstructionFilter = avrFilter
          , isaPseudoInstruction = avrPsuedo
          , isaOperandClassMapping = const []
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
            , isaInstructionFilter = sparcFilter
            , isaPseudoInstruction = const False
            , isaOperandClassMapping = const []
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
