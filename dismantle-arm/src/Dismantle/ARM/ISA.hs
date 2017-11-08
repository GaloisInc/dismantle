{-# LANGUAGE TemplateHaskell #-}
module Dismantle.ARM.ISA (
  isa
  ) where

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Set as S
import Data.Word ( Word8, Word16, Word32, Word64 )

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Dismantle.Tablegen.ISA
import Dismantle.Tablegen.Types
import Dismantle.Tablegen.Parser.Types
  ( Metadata(Metadata)
  , defName
  , defMetadata
  )
import qualified Dismantle.ARM.Operands as ARM

asWord32 :: LBS.ByteString -> Word32
asWord32 = B.runGet B.getWord32be

fromWord32 :: Word32 -> LBS.ByteString
fromWord32 = B.runPut . B.putWord32be

isa :: ISA
isa = ISA { isaName = "ARM"
          , isaTgenBitPreprocess = id
          , isaInputEndianness = Big
          , isaInstructionFilter = armFilter
          , isaPseudoInstruction = armPseudo
          , isaOperandPayloadTypes = armOperandPayloadTypes
          , isaInsnWordFromBytes = 'asWord32
          , isaInsnWordToBytes = 'fromWord32
          , isaInsnAssembleType = ''Word32
          , isaIgnoreOperand = const False
          , isaFormOverrides = overrides
          , isaMapOperandPayloadType = id
          , isaDefaultPrettyVariableValues = []
          , isaPrettyOverrides = []
          , isaUnusedBitsPolicy = Nothing
          }
  where
    gpRegister = OperandPayload { opTypeT = [t| ARM.GPR |]
                                , opConE = Just (varE 'ARM.gpr)
                                , opWordE = Just [| fromIntegral . ARM.unGPR |]
                                }
    qpRegister = OperandPayload { opTypeT = [t| ARM.QPR |]
                                , opConE = Just (varE 'ARM.qpr)
                                , opWordE = Just [| fromIntegral . ARM.unQPR |]
                                }
    qqpRegister = OperandPayload { opTypeT = [t| ARM.QQPR |]
                                 , opConE = Just (varE 'ARM.unQQPR)
                                 , opWordE = Just [| fromIntegral . ARM.unQQPR |]
                                 }
    dpRegister = OperandPayload { opTypeT = [t| ARM.DPR |]
                                , opConE = Just (varE 'ARM.dpr)
                                , opWordE = Just [| fromIntegral . ARM.unDPR |]
                                }
    addrMode3 = OperandPayload { opTypeT = [t| ARM.AddrMode3 |]
                               , opConE = Just (varE 'ARM.mkAddrMode3)
                               , opWordE = Just (varE 'ARM.addrMode3ToBits)
                               }
    addrMode3Offset = OperandPayload { opTypeT = [t| ARM.AM3Offset |]
                                     , opConE = Just (varE 'ARM.mkAM3Offset)
                                     , opWordE = Just (varE 'ARM.am3OffsetToBits)
                                     }
    addrMode5 = OperandPayload { opTypeT = [t| ARM.AddrMode5 |]
                               , opConE = Just (varE 'ARM.mkAddrMode5)
                               , opWordE = Just (varE 'ARM.addrMode5ToBits)
                               }
    addrModeImm12 = OperandPayload { opTypeT = [t| ARM.AddrModeImm12 |]
                                   , opConE = Just (varE 'ARM.mkAddrModeImm12)
                                   , opWordE = Just (varE 'ARM.addrModeImm12ToBits)
                                   }
    modImm = OperandPayload { opTypeT = [t| ARM.ModImm |]
                            , opConE = Just (varE 'ARM.mkModImm)
                            , opWordE = Just (varE 'ARM.modImmToBits)
                            }
    imm5 = OperandPayload { opTypeT = [t| ARM.Imm5 |]
                          , opConE = Just (varE 'ARM.mkImm5)
                          , opWordE = Just (varE 'ARM.imm5ToBits)
                          }
    imm16 = OperandPayload { opTypeT = [t| ARM.Imm16 |]
                           , opConE = Just (varE 'ARM.mkImm16)
                           , opWordE = Just (varE 'ARM.imm16ToBits)
                           }
    predOperand = OperandPayload { opTypeT = [t| ARM.Pred |]
                                 , opConE = Just (varE 'ARM.mkPred)
                                 , opWordE = Just (varE 'ARM.predToBits)
                                 }
    adrLabelOperand = OperandPayload { opTypeT = [t| ARM.AdrLabel |]
                                     , opConE = Just (varE 'ARM.mkAdrLabel)
                                     , opWordE = Just (varE 'ARM.adrLabelToBits)
                                     }
    branchTarget = OperandPayload { opTypeT = [t| ARM.BranchTarget |]
                                  , opConE = Just (varE 'ARM.mkBranchTarget)
                                  , opWordE = Just (varE 'ARM.branchTargetToBits)
                                  }
    branchExecuteTarget = OperandPayload { opTypeT = [t| ARM.BranchExecuteTarget |]
                                         , opConE = Just (varE 'ARM.mkBranchExecuteTarget)
                                         , opWordE = Just (varE 'ARM.branchExecuteTargetToBits)
                                         }
    coprocRegister = OperandPayload { opTypeT = [t| ARM.CoprocRegister |]
                                    , opConE = Just (varE 'ARM.mkCoprocRegister)
                                    , opWordE = Just (varE 'ARM.coprocRegisterToBits)
                                    }
    opcodeOperand = OperandPayload { opTypeT = [t| ARM.Opcode |]
                                   , opConE = Just (varE 'ARM.mkOpcode)
                                   , opWordE = Just (varE 'ARM.opcodeToBits)
                                   }
    word8Operand = OperandPayload { opTypeT = [t| Word8 |]
                                  , opConE = Nothing
                                  , opWordE = Just [| fromIntegral |]
                                  }
    word16Operand = OperandPayload { opTypeT = [t| Word16 |]
                                   , opConE = Nothing
                                   , opWordE = Just [| fromIntegral |]
                                   }
    reglistOperand = OperandPayload { opTypeT = [t| ARM.Reglist |]
                                    , opConE = Just (varE 'ARM.mkRegList)
                                    , opWordE = Just (varE 'ARM.regListToBits)
                                    }
    word24Operand = OperandPayload { opTypeT = [t| Word32 |]
                                   , opConE = Nothing
                                   , opWordE = Nothing
                                   }
    word32Operand = OperandPayload { opTypeT = [t| Word32 |]
                                   , opConE = Nothing
                                   , opWordE = Nothing
                                   }
    ldstSoRegOperand = OperandPayload { opTypeT = [t| ARM.LdstSoReg |]
                                      , opConE = Just (varE 'ARM.mkLdstSoSreg)
                                      , opWordE = Just (varE 'ARM.ldstSoRegToBits)
                                      }
    shiftImm = OperandPayload { opTypeT = [t| ARM.ShiftImm |]
                              , opConE = Just (varE 'ARM.mkShiftImm)
                              , opWordE = Just (varE 'ARM.shiftImmToBits)
                              }
    bit = OperandPayload { opTypeT = [t| ARM.Bit |]
                         , opConE = Just (varE 'ARM.mkBit)
                         , opWordE = Just (varE 'ARM.bitToBits)
                         }
    sBit = OperandPayload { opTypeT = [t| ARM.SBit |]
                          , opConE = Just (varE 'ARM.mkSBit)
                          , opWordE = Just (varE 'ARM.sBitToBits)
                          }
    am2OffsetImm = OperandPayload { opTypeT = [t| ARM.Am2OffsetImm |]
                                  , opConE = Just (varE 'ARM.mkAm2OffsetImm)
                                  , opWordE = Just (varE 'ARM.am2OffsetImmToBits)
                                  }
    am2OffsetReg = OperandPayload { opTypeT = [t| ARM.Am2OffsetReg |]
                                  , opConE = Just (varE 'ARM.mkAm2OffsetReg)
                                  , opWordE = Just (varE 'ARM.am2OffsetRegToBits)
                                  }
    regWithAdd = OperandPayload { opTypeT = [t| ARM.RegWithAdd |]
                                , opConE = Just (varE 'ARM.mkRegWithAdd)
                                , opWordE = Just (varE 'ARM.regWithAddToBits)
                                }
    msrMask = OperandPayload { opTypeT = [t| ARM.MSRMask |]
                             , opConE = Just (varE 'ARM.mkMSRMask)
                             , opWordE = Just (varE 'ARM.msrMaskToBits)
                             }
    soRegImm = OperandPayload { opTypeT = [t| ARM.SoRegImm |]
                              , opConE = Just (varE 'ARM.mkSoRegImm)
                              , opWordE = Just (varE 'ARM.soRegImmToBits)
                              }
    soRegReg = OperandPayload { opTypeT = [t| ARM.SoRegReg |]
                              , opConE = Just (varE 'ARM.mkSoRegReg)
                              , opWordE = Just (varE 'ARM.soRegRegToBits)
                              }
    svcOperand = OperandPayload { opTypeT = [t| ARM.SvcOperand |]
                                , opConE = Just (varE 'ARM.mkSvcOperand)
                                , opWordE = Just (varE 'ARM.svcOperandToBits)
                                }
    imm8s4 = OperandPayload { opTypeT = [t| ARM.Imm8S4 |]
                            , opConE = Just (varE 'ARM.mkImm8s4)
                            , opWordE = Just (varE 'ARM.imm8s4ToBits)
                            }

    armOperandPayloadTypes =
        [ ("Addr_offset_none"  , gpRegister)
        , ("Addrmode3"         , addrMode3)
        , ("Addrmode3_pre"     , addrMode3)
        , ("Addrmode5"         , addrMode5)
        , ("Addrmode5_pre"     , addrMode5)
        , ("Addrmode_imm12"    , addrModeImm12)
        , ("Addrmode_imm12_pre", addrModeImm12)
        , ("Adrlabel"          , adrLabelOperand)
        , ("Am2offset_imm"     , am2OffsetImm)
        , ("Am2offset_reg"     , am2OffsetReg)
        , ("Am3offset"         , addrMode3Offset)
        , ("Arm_bl_target"     , branchTarget)
        , ("Arm_blx_target"    , branchExecuteTarget)
        , ("Arm_br_target"     , branchTarget)
        , ("Bf_inv_mask_imm"   , word16Operand)
        , ("C_imm"             , coprocRegister)
        , ("Cc_out"            , sBit)
        , ("Coproc_option_imm" , word8Operand)
        , ("Dpr"               , dpRegister)
        , ("GPR"               , gpRegister)
        , ("GPRPairOp"         , gpRegister)
        , ("GPRnopc"           , gpRegister)
        , ("Iflags_op"         , word8Operand)
        , ("Imm0_1"            , bit)
        , ("Imm0_15"           , opcodeOperand)
        , ("Imm0_31"           , imm5)
        , ("Imm0_65535"        , imm16)
        , ("Imm0_7"            , opcodeOperand)
        , ("Imm0_239"          , word8Operand)
        , ("Imm0_65535_expr"   , word16Operand)
        , ("Imm1_16"           , word8Operand)
        , ("Imm1_32"           , imm5)
        , ("Imm24b"            , svcOperand)
        , ("Imod_op"           , word8Operand)
        , ("Instsyncb_opt"     , word8Operand)
        , ("Ldst_so_reg"       , ldstSoRegOperand)
        , ("Memb_opt"          , word8Operand)
        , ("Mod_imm"           , modImm)
        , ("Msr_mask"          , msrMask)
        , ("P_imm"             , word8Operand)
        , ("Pkh_asr_amt"       , word8Operand)
        , ("Pkh_lsl_amt"       , word8Operand)
        , ("Postidx_imm8"      , addrMode3Offset)
        , ("Postidx_imm8s4"    , imm8s4)
        , ("Postidx_reg"       , regWithAdd)
        , ("Pred"              , predOperand)
        , ("Qpr"               , qpRegister)
        , ("Qqpr"              , qqpRegister)
        , ("GPRwithAPSR"       , gpRegister)
        , ("Reglist"           , reglistOperand)
        , ("Rot_imm"           , word8Operand)
        , ("Setend_op"         , bit)
        , ("Shift_imm"         , shiftImm)
        -- This operand type is only used for the MOVsi def, which is a
        -- pseudo-instruction for various other types of operations (see
        -- the ARM ARM A8.8.105 MOV (shifted register)).
        , ("Shift_so_reg_imm"  , word16Operand)
        -- This operand type is only used for the MOVsr def, which is a
        -- pseudo-instruction for various other types of operations (see
        -- the ARM ARM A8.8.105 MOV (shifted register)).
        , ("Shift_so_reg_reg"  , word16Operand)
        , ("So_reg_imm"        , soRegImm)
        , ("So_reg_reg"        , soRegReg)
        , ("TcGPR"             , gpRegister)
        , ("Unpredictable"     , word32Operand)
        ]

    armFilter = hasNamedString "Namespace" "ARM" &&&
                hasNamedString "DecoderNamespace" "ARM" &&&
                (not . isPseudo) &&&
                (not . ignoredDef) &&&
                (not . ignoredMetadata)

    armPseudo = idPseudo

    pseudoInstructionNames =
        [
        ]

    ignoredDef d = defName d `elem`
        [
        -- Thumb instructions not properly marked as such:
          "tInt_eh_sjlj_longjmp"
        , "tInt_WIN_eh_sjlj_longjmp"

        -- Instructions with more generic alternatives
        , "BX" -- see BX_pred
        , "TSTrr" -- See TSTrsi
        , "SETPAN"
        ]

    ignoredMetadataNames = S.fromList $ Metadata <$>
        [ "PseudoNLdSt"
        , "PseudoNeonI"
        , "PseudoVFPLdStM"
        ]

    ignoredMetadata d = not $ S.null $
                        S.intersection (S.fromList $ defMetadata d)
                                       ignoredMetadataNames

    overrides =
        [ ("AES2Op",         FormOverride [("src", Ignore)])
        , ("BFC",            FormOverride [("src", Ignore)])
        , ("BFI",            FormOverride [("src", Ignore)])
        , ("DA_UPD",         FormOverride [("wb", Ignore)])
        , ("DB_UPD",         FormOverride [("wb", Ignore)])
        , ("IA_UPD",         FormOverride [("wb", Ignore)])
        , ("IB_UPD",         FormOverride [("wb", Ignore)])
        , ("N3SHA3Op",       FormOverride [("src", Ignore)])
        , ("SMLAL",          FormOverride [("RLo", Ignore), ("RHi", Ignore)])
        , ("MOVTi16",        FormOverride [("src", Ignore)])
        , ("STMIA_UPD",      FormOverride [("wb", Ignore)])
        , ("STMIB_UPD",      FormOverride [("wb", Ignore)])
        , ("STRBT_POST_IMM", FormOverride [("Rn_wb", Ignore)])
        , ("STRBT_POST_REG", FormOverride [("Rn_wb", Ignore)])
        , ("LDRBT_POST_IMM", FormOverride [("Rn_wb", Ignore)])
        , ("LDRBT_POST_REG", FormOverride [("Rn_wb", Ignore)])
        , ("LDRT_POST_IMM",  FormOverride [("Rn_wb", Ignore)])
        , ("LDRT_POST_REG",  FormOverride [("Rn_wb", Ignore)])
        , ("STRD",           FormOverride [("Rt2", Ignore)])
        , ("STRD_POST",      FormOverride [("Rt2", Ignore), ("Rn_wb", Ignore)])
        , ("STRD_PRE",       FormOverride [("Rt2", Ignore), ("Rn_wb", Ignore)])
        , ("LDRD",           FormOverride [("Rt2", Ignore)])
        , ("LDRD_POST",      FormOverride [("Rt2", Ignore), ("Rn_wb", Ignore)])
        , ("LDRD_PRE",       FormOverride [("Rt2", Ignore), ("Rn_wb", Ignore)])
        , ("STRHTi",         FormOverride [("base_wb", Ignore)])
        , ("STRHTr",         FormOverride [("base_wb", Ignore)])
        , ("LDRHTi",         FormOverride [("base_wb", Ignore)])
        , ("LDRHTr",         FormOverride [("base_wb", Ignore)])
        , ("LDRSBTi",        FormOverride [("base_wb", Ignore)])
        , ("LDRSBTr",        FormOverride [("base_wb", Ignore)])
        , ("LDRSHTi",        FormOverride [("base_wb", Ignore)])
        , ("LDRSHTr",        FormOverride [("base_wb", Ignore)])
        , ("LDRSB_POST",     FormOverride [("Rn_wb", Ignore)])
        , ("LDRSB_PRE",      FormOverride [("Rn_wb", Ignore)])
        , ("LDRSH_POST",     FormOverride [("Rn_wb", Ignore)])
        , ("LDRSH_PRE",      FormOverride [("Rn_wb", Ignore)])
        , ("STRH_POST",      FormOverride [("Rn_wb", Ignore)])
        , ("STRH_PRE",       FormOverride [("Rn_wb", Ignore)])
        , ("LDRH_POST",      FormOverride [("Rn_wb", Ignore)])
        , ("LDRH_PRE",       FormOverride [("Rn_wb", Ignore)])
        , ("STRT_POST_IMM" , FormOverride [("Rn_wb", Ignore)])
        , ("STRT_POST_REG" , FormOverride [("Rn_wb", Ignore)])
        , ("UMAAL",          FormOverride [("RLo", Ignore), ("RHi", Ignore)])
        , ("UMLAL",          FormOverride [("RLo", Ignore), ("RHi", Ignore)])
        , ("XDB_UPD",        FormOverride [("wb", Ignore)])
        , ("_POST_IMM",      FormOverride [("Rn_wb", Ignore)])
        , ("_POST_REG",      FormOverride [("Rn_wb", Ignore)])
        , ("_PRE_IMM",       FormOverride [("Rn_wb", Ignore)])
        , ("_PRE_REG",       FormOverride [("Rn_wb", Ignore)])
        , ("MRSbanked",      FormOverride [("banked", Ignore)])
        , ("MSRbanked",      FormOverride [("banked", Ignore)])
        ]
