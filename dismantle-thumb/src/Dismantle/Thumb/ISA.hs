{-# LANGUAGE TemplateHaskell #-}
module Dismantle.Thumb.ISA (
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
import qualified Dismantle.Thumb.Operands as Thumb

asWord32 :: LBS.ByteString -> Word32
asWord32 = B.runGet B.getWord32be

fromWord32 :: Word32 -> LBS.ByteString
fromWord32 = B.runPut . B.putWord32be

isa :: ISA
isa = ISA { isaName = "Thumb"
          , isaTgenBitPreprocess = id
          , isaInputEndianness = Big
          , isaInstructionFilter = thumbFilter
          , isaPseudoInstruction = thumbPseudo
          , isaOperandPayloadTypes = thumbOperandPayloadTypes
          , isaInsnWordFromBytes = 'asWord32
          , isaInsnWordToBytes = 'fromWord32
          , isaInsnAssembleType = ''Word32
          , isaIgnoreOperand = const False
          , isaFormOverrides = overrides
          , isaMapOperandPayloadType = id
          }
  where
    gpRegister = OperandPayload { opTypeT = [t| Thumb.GPR |]
                                , opConE = Just (varE 'Thumb.gpr)
                                , opWordE = Just [| fromIntegral . Thumb.unGPR |]
                                }
    lowGpRegister = OperandPayload { opTypeT = [t| Thumb.LowGPR |]
                                   , opConE = Just (varE 'Thumb.lowGpr)
                                   , opWordE = Just [| fromIntegral . Thumb.unLowGPR |]
                                   }
    qpRegister = OperandPayload { opTypeT = [t| Thumb.QPR |]
                                , opConE = Just (varE 'Thumb.qpr)
                                , opWordE = Just [| fromIntegral . Thumb.unQPR |]
                                }
    qqpRegister = OperandPayload { opTypeT = [t| Thumb.QQPR |]
                                 , opConE = Just (varE 'Thumb.unQQPR)
                                 , opWordE = Just [| fromIntegral . Thumb.unQQPR |]
                                 }
    dpRegister = OperandPayload { opTypeT = [t| Thumb.DPR |]
                                , opConE = Just (varE 'Thumb.dpr)
                                , opWordE = Just [| fromIntegral . Thumb.unDPR |]
                                }
    addrMode3 = OperandPayload { opTypeT = [t| Thumb.AddrMode3 |]
                               , opConE = Just (varE 'Thumb.mkAddrMode3)
                               , opWordE = Just (varE 'Thumb.addrMode3ToBits)
                               }
    addrMode3Offset = OperandPayload { opTypeT = [t| Thumb.AM3Offset |]
                                     , opConE = Just (varE 'Thumb.mkAM3Offset)
                                     , opWordE = Just (varE 'Thumb.am3OffsetToBits)
                                     }
    addrMode5 = OperandPayload { opTypeT = [t| Thumb.AddrMode5 |]
                               , opConE = Just (varE 'Thumb.mkAddrMode5)
                               , opWordE = Just (varE 'Thumb.addrMode5ToBits)
                               }
    addrModeImm12 = OperandPayload { opTypeT = [t| Thumb.AddrModeImm12 |]
                                   , opConE = Just (varE 'Thumb.mkAddrModeImm12)
                                   , opWordE = Just (varE 'Thumb.addrModeImm12ToBits)
                                   }
    addrModeIs1 = OperandPayload { opTypeT = [t| Thumb.AddrModeIs1 |]
                                 , opConE = Just (varE 'Thumb.mkAddrModeIs1)
                                 , opWordE = Just (varE 'Thumb.addrModeIs1ToBits)
                                 }
    addrModeIs2 = OperandPayload { opTypeT = [t| Thumb.AddrModeIs2 |]
                                 , opConE = Just (varE 'Thumb.mkAddrModeIs2)
                                 , opWordE = Just (varE 'Thumb.addrModeIs2ToBits)
                                 }
    addrModeIs4 = OperandPayload { opTypeT = [t| Thumb.AddrModeIs4 |]
                                 , opConE = Just (varE 'Thumb.mkAddrModeIs4)
                                 , opWordE = Just (varE 'Thumb.addrModeIs4ToBits)
                                 }
    modImm = OperandPayload { opTypeT = [t| Thumb.ModImm |]
                            , opConE = Just (varE 'Thumb.mkModImm)
                            , opWordE = Just (varE 'Thumb.modImmToBits)
                            }
    imm5 = OperandPayload { opTypeT = [t| Thumb.Imm5 |]
                          , opConE = Just (varE 'Thumb.mkImm5)
                          , opWordE = Just (varE 'Thumb.imm5ToBits)
                          }
    imm16 = OperandPayload { opTypeT = [t| Thumb.Imm16 |]
                           , opConE = Just (varE 'Thumb.mkImm16)
                           , opWordE = Just (varE 'Thumb.imm16ToBits)
                           }
    predOperand = OperandPayload { opTypeT = [t| Thumb.Pred |]
                                 , opConE = Just (varE 'Thumb.mkPred)
                                 , opWordE = Just (varE 'Thumb.predToBits)
                                 }
    adrLabelOperand = OperandPayload { opTypeT = [t| Thumb.AdrLabel |]
                                     , opConE = Just (varE 'Thumb.mkAdrLabel)
                                     , opWordE = Just (varE 'Thumb.adrLabelToBits)
                                     }
    branchTarget = OperandPayload { opTypeT = [t| Thumb.BranchTarget |]
                                  , opConE = Just (varE 'Thumb.mkBranchTarget)
                                  , opWordE = Just (varE 'Thumb.branchTargetToBits)
                                  }
    branchExecuteTarget = OperandPayload { opTypeT = [t| Thumb.BranchExecuteTarget |]
                                         , opConE = Just (varE 'Thumb.mkBranchExecuteTarget)
                                         , opWordE = Just (varE 'Thumb.branchExecuteTargetToBits)
                                         }
    coprocRegister = OperandPayload { opTypeT = [t| Thumb.CoprocRegister |]
                                    , opConE = Just (varE 'Thumb.mkCoprocRegister)
                                    , opWordE = Just (varE 'Thumb.coprocRegisterToBits)
                                    }
    opcodeOperand = OperandPayload { opTypeT = [t| Thumb.Opcode |]
                                   , opConE = Just (varE 'Thumb.mkOpcode)
                                   , opWordE = Just (varE 'Thumb.opcodeToBits)
                                   }
    word8Operand = OperandPayload { opTypeT = [t| Word8 |]
                                  , opConE = Nothing
                                  , opWordE = Just [| fromIntegral |]
                                  }
    word16Operand = OperandPayload { opTypeT = [t| Word16 |]
                                   , opConE = Nothing
                                   , opWordE = Just [| fromIntegral |]
                                   }
    reglistOperand = OperandPayload { opTypeT = [t| Thumb.Reglist |]
                                    , opConE = Just (varE 'Thumb.mkRegList)
                                    , opWordE = Just (varE 'Thumb.regListToBits)
                                    }
    word24Operand = OperandPayload { opTypeT = [t| Word32 |]
                                   , opConE = Nothing
                                   , opWordE = Nothing
                                   }
    word32Operand = OperandPayload { opTypeT = [t| Word32 |]
                                   , opConE = Nothing
                                   , opWordE = Nothing
                                   }
    ldstSoRegOperand = OperandPayload { opTypeT = [t| Thumb.LdstSoReg |]
                                      , opConE = Just (varE 'Thumb.mkLdstSoSreg)
                                      , opWordE = Just (varE 'Thumb.ldstSoRegToBits)
                                      }
    shiftImm = OperandPayload { opTypeT = [t| Thumb.ShiftImm |]
                              , opConE = Just (varE 'Thumb.mkShiftImm)
                              , opWordE = Just (varE 'Thumb.shiftImmToBits)
                              }
    bit = OperandPayload { opTypeT = [t| Thumb.Bit |]
                         , opConE = Just (varE 'Thumb.mkBit)
                         , opWordE = Just (varE 'Thumb.bitToBits)
                         }
    sBit = OperandPayload { opTypeT = [t| Thumb.SBit |]
                          , opConE = Just (varE 'Thumb.mkSBit)
                          , opWordE = Just (varE 'Thumb.sBitToBits)
                          }
    am2OffsetImm = OperandPayload { opTypeT = [t| Thumb.Am2OffsetImm |]
                                  , opConE = Just (varE 'Thumb.mkAm2OffsetImm)
                                  , opWordE = Just (varE 'Thumb.am2OffsetImmToBits)
                                  }
    am2OffsetReg = OperandPayload { opTypeT = [t| Thumb.Am2OffsetReg |]
                                  , opConE = Just (varE 'Thumb.mkAm2OffsetReg)
                                  , opWordE = Just (varE 'Thumb.am2OffsetRegToBits)
                                  }
    regWithAdd = OperandPayload { opTypeT = [t| Thumb.RegWithAdd |]
                                , opConE = Just (varE 'Thumb.mkRegWithAdd)
                                , opWordE = Just (varE 'Thumb.regWithAddToBits)
                                }
    msrMask = OperandPayload { opTypeT = [t| Thumb.MSRMask |]
                             , opConE = Just (varE 'Thumb.mkMSRMask)
                             , opWordE = Just (varE 'Thumb.msrMaskToBits)
                             }
    soRegImm = OperandPayload { opTypeT = [t| Thumb.SoRegImm |]
                              , opConE = Just (varE 'Thumb.mkSoRegImm)
                              , opWordE = Just (varE 'Thumb.soRegImmToBits)
                              }
    soRegReg = OperandPayload { opTypeT = [t| Thumb.SoRegReg |]
                              , opConE = Just (varE 'Thumb.mkSoRegReg)
                              , opWordE = Just (varE 'Thumb.soRegRegToBits)
                              }
    svcOperand = OperandPayload { opTypeT = [t| Thumb.SvcOperand |]
                                , opConE = Just (varE 'Thumb.mkSvcOperand)
                                , opWordE = Just (varE 'Thumb.svcOperandToBits)
                                }
    imm8s4 = OperandPayload { opTypeT = [t| Thumb.Imm8S4 |]
                            , opConE = Just (varE 'Thumb.mkImm8s4)
                            , opWordE = Just (varE 'Thumb.imm8s4ToBits)
                            }

    thumbOperandPayloadTypes =
        [
          ("GPR"               , gpRegister)
        , ("TGPR"              , lowGpRegister)
        -- [ ("Addr_offset_none"  , gpRegister)
        -- , ("Addrmode3"         , addrMode3)
        -- , ("Addrmode3_pre"     , addrMode3)
        -- , ("Addrmode5"         , addrMode5)
        -- , ("Addrmode5_pre"     , addrMode5)
        -- , ("Addrmode_imm12"    , addrModeImm12)
        -- , ("Addrmode_imm12_pre", addrModeImm12)
        -- , ("Adrlabel"          , adrLabelOperand)
        -- , ("Am2offset_imm"     , am2OffsetImm)
        -- , ("Am2offset_reg"     , am2OffsetReg)
        -- , ("Am3offset"         , addrMode3Offset)
        , ("T_addrmode_is1"       , addrModeIs1)
        , ("T_addrmode_is2"       , addrModeIs2)
        , ("T_addrmode_is4"       , addrModeIs4)
        -- , ("Arm_bl_target"     , branchTarget)
        -- , ("Arm_blx_target"    , branchExecuteTarget)
        -- , ("Arm_br_target"     , branchTarget)
        -- , ("Bf_inv_mask_imm"   , word16Operand)
        -- , ("C_imm"             , coprocRegister)
        -- , ("Cc_out"            , sBit)
        -- , ("Coproc_option_imm" , word8Operand)
        -- , ("Dpr"               , dpRegister)
        , ("Thumb_bcc_target"  , word8Operand)
        , ("Thumb_cb_target"   , word8Operand)
        -- , ("GPRPairOp"         , gpRegister)
        -- , ("GPRnopc"           , gpRegister)
        , ("Iflags_op"         , word8Operand)
        , ("Imm0_1"            , bit)
        -- , ("Imm0_15"           , opcodeOperand)
        -- , ("Imm0_31"           , imm5)
        , ("Imm0_63"           , word8Operand)
        -- , ("Imm0_65535"        , imm16)
        -- , ("Imm0_7"            , opcodeOperand)
        -- , ("Imm0_239"          , word8Operand)
        , ("Imm0_255"          , word8Operand)
        -- , ("Imm0_65535_expr"   , word16Operand)
        -- , ("Imm1_16"           , word8Operand)
        -- , ("Imm1_32"           , imm5)
        -- , ("Imm24b"            , svcOperand)
        , ("Imod_op"           , word8Operand)
        -- , ("Instsyncb_opt"     , word8Operand)
        -- , ("Ldst_so_reg"       , ldstSoRegOperand)
        -- , ("Memb_opt"          , word8Operand)
        -- , ("Mod_imm"           , modImm)
        -- , ("Msr_mask"          , msrMask)
        -- , ("P_imm"             , word8Operand)
        -- , ("Pkh_asr_amt"       , word8Operand)
        -- , ("Pkh_lsl_amt"       , word8Operand)
        -- , ("Postidx_imm8"      , addrMode3Offset)
        -- , ("Postidx_imm8s4"    , imm8s4)
        -- , ("Postidx_reg"       , regWithAdd)
        , ("Pred"              , predOperand)
        -- , ("Qpr"               , qpRegister)
        -- , ("Qqpr"              , qqpRegister)
        -- , ("GPRwithAPSR"       , gpRegister)
        , ("Reglist"           , reglistOperand)
        -- , ("Rot_imm"           , word8Operand)
        , ("Setend_op"         , bit)
        -- , ("Shift_imm"         , shiftImm)
        -- -- This operand type is only used for the MOVsi def, which is a
        -- -- pseudo-instruction for various other types of operations (see
        -- -- the ARM ARM A8.8.105 MOV (shifted register)).
        -- , ("Shift_so_reg_imm"  , word16Operand)
        -- -- This operand type is only used for the MOVsr def, which is a
        -- -- pseudo-instruction for various other types of operations (see
        -- -- the ARM ARM A8.8.105 MOV (shifted register)).
        -- , ("Shift_so_reg_reg"  , word16Operand)
        -- , ("So_reg_imm"        , soRegImm)
        -- , ("So_reg_reg"        , soRegReg)
        -- , ("TcGPR"             , gpRegister)
        , ("Unpredictable"     , word32Operand)
        ]

    thumbFilter = hasNamedString "Namespace" "ARM" &&&
                  hasNamedString "DecoderNamespace" "Thumb" &&&
                  (not . isPseudo) &&&
                  (not . ignoredDef) &&&
                  (not . ignoredMetadata)

    thumbPseudo = idPseudo

    pseudoInstructionNames =
        [
        ]

    ignoredDef d = defName d `elem`
        [
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
        [ ("tUXTH",      FormOverride [("p", Ignore)])
        , ("tUXTB",      FormOverride [("p", Ignore)])
        , ("tSXTH",      FormOverride [("p", Ignore)])
        , ("tSXTB",      FormOverride [("p", Ignore)])
        , ("tTST",       FormOverride [("p", Ignore)])
        , ("tSTRr",      FormOverride [("p", Ignore)])
        , ("tSTRi",      FormOverride [("p", Ignore)])
        , ("tSTRspi",    FormOverride [("p", Ignore)])
        , ("tSTRBi",     FormOverride [("p", Ignore)])
        , ("tSTRBr",     FormOverride [("p", Ignore)])
        , ("tSTRHi",     FormOverride [("p", Ignore)])
        , ("tSTRHr",     FormOverride [("p", Ignore)])
        , ("tSVC",       FormOverride [("p", Ignore)])
        , ("tSUBspi",    FormOverride [("Rdn", Ignore)])
        , ("tSTMIA_UPD", FormOverride [("wb", Ignore)])
        , ("tLDMIA",     FormOverride [("p", Ignore)])
        , ("tLDRBi",     FormOverride [("p", Ignore)])
        , ("tLDRBr",     FormOverride [("p", Ignore)])
        , ("tLDRHi",     FormOverride [("p", Ignore)])
        , ("tLDRHr",     FormOverride [("p", Ignore)])
        , ("tLDRSB",     FormOverride [("p", Ignore)])
        , ("tLDRSH",     FormOverride [("p", Ignore)])
        , ("tLDRi",      FormOverride [("p", Ignore)])
        , ("tLDRpci",    FormOverride [("p", Ignore)])
        , ("tLDRr",      FormOverride [("p", Ignore)])
        , ("tLDRspi",    FormOverride [("p", Ignore)])
        , ("tMOVr",      FormOverride [("p", Ignore)])
        ]
