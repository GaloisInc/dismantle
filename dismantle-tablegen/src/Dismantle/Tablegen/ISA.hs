module Dismantle.Tablegen.ISA (
  ISA(..),
  arm,
  thumb,
  aarch64,
  ppc,
  mips,
  avr,
  sparc
  ) where

import qualified GHC.Err.Located as L

import Dismantle.Tablegen.Types

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
      }

arm :: ISA
arm = ISA { isaName = "ARM"
          , isaInstructionFilter = armFilter
          , isaPseudoInstruction = const False
          }
  where
    armFilter i = idDecoder i == "ARM" && idNamespace i == "ARM" && not (idPseudo i)

thumb :: ISA
thumb = ISA { isaName = "Thumb"
            , isaInstructionFilter = thumbFilter
            , isaPseudoInstruction = const False
            }
  where
    thumbFilter i = idDecoder i == "Thumb" && idNamespace i == "ARM" && not (idPseudo i)

aarch64 :: ISA
aarch64 = ISA { isaName = "AArch64"
              , isaInstructionFilter = aarch64Filter
              , isaPseudoInstruction = const False
              }
  where
    aarch64Filter i = idNamespace i == "AArch64" && not (idPseudo i)

ppc :: ISA
ppc = ISA { isaName = "PPC"
          , isaInstructionFilter = ppcFilter
          , isaPseudoInstruction = const False
          }
  where
    ppcFilter i = idNamespace i == "PPC" && not (idPseudo i)
    -- ppcFieldType t =
    --   case t of
    --     "crbitrc" -> Register
    --     "vrrc" -> Register
    --     "tlsreg" -> Register
    --     "tlsreg32" -> Register
    --     "tlsgd32" -> Register
    --     "gprc" -> Register
    --     "g8rc" -> Register
    --     "g8rc_nox0" -> Register
    --     "gprc_nor0" -> Register
    --     "tocentry" -> Register -- ?? this is some table lookup thing, so memory?
    --     "i32imm" -> Immediate
    --     "s32imm" -> Immediate
    --     "s16imm" -> Immediate
    --     "s16imm_64" -> Immediate
    --     "s17imm" -> Immediate
    --     "s17imm64" -> Immediate
    --     "u16imm" -> Immediate
    --     "u16imm_64" -> Immediate
    --     "u17imm" -> Immediate
    --     "u17imm64" -> Immediate
    --     "u5imm" -> Immediate
    --     "s5imm" -> Immediate
    --     "u4imm" -> Immediate
    --     "s4imm" -> Immediate
    --     "u1imm" -> Immediate
    --     "memrr" -> Memory
    --     "memrix" -> Memory
    --     "memri" -> Memory
    --     "condbrtarget" -> Offset
    --     "abscondbrtarget" -> Address
    --     "crrc" -> Register
    --     "f8rc" -> Register
    --     "f4rc" -> Register
    --     "vsfrc" -> Register
    --     "vssrc" -> Register
    --     _ -> L.error ("Unexpected PPC field class: " ++ t)

mips :: ISA
mips = ISA { isaName = "Mips"
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
    avrFilter i = idNamespace i == "AVR" && not (idPseudo i)
    avrPsuedo i = idMnemonic i `elem` [ "CBRRdK" -- Clear bits, equivalent to an ANDi
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
            }
  where
    sparcFilter i = idNamespace i == "SP" && idDecoder i == "Sparc" && not (idPseudo i)
