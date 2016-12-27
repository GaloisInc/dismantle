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
      }

arm :: ISA
arm = ISA { isaName = "ARM"
          , isaInstructionFilter = armFilter
          }
  where
    armFilter i = idDecoder i == "ARM" && idNamespace i == "ARM" && not (idPseudo i)

thumb :: ISA
thumb = ISA { isaName = "Thumb"
            , isaInstructionFilter = thumbFilter
            }
  where
    thumbFilter i = idDecoder i == "Thumb" && idNamespace i == "ARM" && not (idPseudo i)

aarch64 :: ISA
aarch64 = ISA { isaName = "AArch64"
              , isaInstructionFilter = aarch64Filter
              }
  where
    aarch64Filter i = idNamespace i == "AArch64" && not (idPseudo i)

ppc :: ISA
ppc = ISA { isaName = "PPC"
          , isaInstructionFilter = ppcFilter
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
           }
  where
    mipsFilter i = idDecoder i == "Mips" && idNamespace i == "Mips" && not (idPseudo i)

avr :: ISA
avr = ISA { isaName = "AVR"
          , isaInstructionFilter = avrFilter
          }
  where
    avrFilter i = idNamespace i == "AVR" && not (idPseudo i)

sparc :: ISA
sparc = ISA { isaName = "Sparc"
            , isaInstructionFilter = sparcFilter
            }
  where
    sparcFilter i = idNamespace i == "SP" && idDecoder i == "Sparc" && not (idPseudo i)
