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
          , isaPseudoInstruction = ppcPseudo
          }
  where
    ppcFilter i = idNamespace i == "PPC" && idDecoder i == ""
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
            , isaInstructionFilter = sparcFilter
            , isaPseudoInstruction = const False
            }
  where
    sparcFilter i = idNamespace i == "SP" && idDecoder i == "Sparc" && not (idPseudo i)
