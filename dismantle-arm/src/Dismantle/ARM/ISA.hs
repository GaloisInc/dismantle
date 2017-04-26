{-# LANGUAGE TemplateHaskell #-}
module Dismantle.ARM.ISA (
  isa
  ) where

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import Data.Word ( Word8, Word16, Word32, Word64 )

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Dismantle.Tablegen.ISA
import Dismantle.Tablegen.Types
import qualified Dismantle.ARM.Operands as ARM

asWord32 :: LBS.ByteString -> Word32
asWord32 = B.runGet B.getWord32be

fromWord32 :: Word32 -> LBS.ByteString
fromWord32 = B.runPut . B.putWord32be

isa :: ISA
isa = ISA { isaName = "ARM"
          , isaEndianness = Big
          , isaInstructionFilter = armFilter
          , isaPseudoInstruction = armPseudo
          , isaOperandPayloadTypes = armOperandPayloadTypes
          , isaInsnWordFromBytes = 'asWord32
          , isaInsnWordToBytes = 'fromWord32
          , isaInsnAssembleType = ''Word32
          , isaIgnoreOperand = const False
          , isaFormOverrides = overrides
          }
  where
    gpRegister = OperandPayload { opTypeName = ''ARM.GPR
                                , opConE = Just (varE 'ARM.gpr)
                                , opWordE = Just [| fromIntegral . ARM.unGPR |]
                                }
    qpRegister = OperandPayload { opTypeName = ''ARM.QPR
                                , opConE = Just (varE 'ARM.qpr)
                                , opWordE = Just [| fromIntegral . ARM.unQPR |]
                                }
    qqpRegister = OperandPayload { opTypeName = ''ARM.QQPR
                                 , opConE = Just (varE 'ARM.unQQPR)
                                 , opWordE = Just [| fromIntegral . ARM.unQQPR |]
                                 }
    dpRegister = OperandPayload { opTypeName = ''ARM.DPR
                                , opConE = Just (varE 'ARM.dpr)
                                , opWordE = Just [| fromIntegral . ARM.unDPR |]
                                }
    addrMode3 = OperandPayload { opTypeName = ''ARM.AddrMode3
                               , opConE = Just (varE 'ARM.mkAddrMode3)
                               , opWordE = Just (varE 'ARM.addrMode3ToBits)
                               }
    addrMode5 = OperandPayload { opTypeName = ''ARM.AddrMode5
                               , opConE = Just (varE 'ARM.mkAddrMode5)
                               , opWordE = Just (varE 'ARM.addrMode5ToBits)
                               }
    addrModeImm12 = OperandPayload { opTypeName = ''ARM.AddrModeImm12
                                   , opConE = Just (varE 'ARM.mkAddrModeImm12)
                                   , opWordE = Just (varE 'ARM.addrModeImm12ToBits)
                                   }
    imm12 = OperandPayload { opTypeName = ''ARM.Imm12
                           , opConE = Just (varE 'ARM.mkImm12)
                           , opWordE = Just (varE 'ARM.imm12ToBits)
                           }
    imm5 = OperandPayload { opTypeName = ''ARM.Imm5
                          , opConE = Just (varE 'ARM.mkImm5)
                          , opWordE = Just (varE 'ARM.imm5ToBits)
                          }
    imm16 = OperandPayload { opTypeName = ''ARM.Imm16
                           , opConE = Just (varE 'ARM.mkImm16)
                           , opWordE = Just (varE 'ARM.imm16ToBits)
                           }
    predOperand = OperandPayload { opTypeName = ''ARM.Pred
                                 , opConE = Just (varE 'ARM.mkPred)
                                 , opWordE = Just (varE 'ARM.predToBits)
                                 }
    sBitOperand = OperandPayload { opTypeName = ''ARM.SBit
                                 , opConE = Just (varE 'ARM.mkSBit)
                                 , opWordE = Just (varE 'ARM.sBitToBits)
                                 }
    adrLabelOperand = OperandPayload { opTypeName = ''ARM.AdrLabel
                                     , opConE = Just (varE 'ARM.mkAdrLabel)
                                     , opWordE = Just (varE 'ARM.adrLabelToBits)
                                     }
    branchTarget = OperandPayload { opTypeName = ''ARM.BranchTarget
                                  , opConE = Just (varE 'ARM.mkBranchTarget)
                                  , opWordE = Just (varE 'ARM.branchTargetToBits)
                                  }
    branchExecuteTarget = OperandPayload { opTypeName = ''ARM.BranchExecuteTarget
                                         , opConE = Just (varE 'ARM.mkBranchExecuteTarget)
                                         , opWordE = Just (varE 'ARM.branchExecuteTargetToBits)
                                         }
    coprocRegister = OperandPayload { opTypeName = ''ARM.CoprocRegister
                                    , opConE = Just (varE 'ARM.mkCoprocRegister)
                                    , opWordE = Just (varE 'ARM.coprocRegisterToBits)
                                    }
    opcodeOperand = OperandPayload { opTypeName = ''ARM.Opcode
                                   , opConE = Just (varE 'ARM.mkOpcode)
                                   , opWordE = Just (varE 'ARM.opcodeToBits)
                                   }
    word8Operand = OperandPayload { opTypeName = ''Word8
                                  , opConE = Nothing
                                  , opWordE = Just [| fromIntegral |]
                                  }
    word16Operand = OperandPayload { opTypeName = ''Word16
                                   , opConE = Nothing
                                   , opWordE = Just [| fromIntegral |]
                                   }
    word24Operand = OperandPayload { opTypeName = ''Word32
                                   , opConE = Nothing
                                   , opWordE = Nothing
                                   }
    ldstSoRegOperand = OperandPayload { opTypeName = ''ARM.LdstSoReg
                                      , opConE = Just (varE 'ARM.mkLdstSoSreg)
                                      , opWordE = Just (varE 'ARM.ldstSoRegToBits)
                                      }
    shiftImm = OperandPayload { opTypeName = ''ARM.ShiftImm
                              , opConE = Just (varE 'ARM.mkShiftImm)
                              , opWordE = Just (varE 'ARM.shiftImmToBits)
                              }
    bit = OperandPayload { opTypeName = ''ARM.Bit
                         , opConE = Just (varE 'ARM.mkBit)
                         , opWordE = Just (varE 'ARM.bitToBits)
                         }
    am2OffsetImm = OperandPayload { opTypeName = ''ARM.Am2OffsetImm
                                  , opConE = Just (varE 'ARM.mkAm2OffsetImm)
                                  , opWordE = Just (varE 'ARM.am2OffsetImmToBits)
                                  }

    armOperandPayloadTypes =
        [ ("Dpr"               , dpRegister)
        , ("GPR"               , gpRegister)
        , ("GPRPairOp"         , gpRegister)
        , ("GPRnopc"           , gpRegister)
        , ("Qpr"               , qpRegister)
        , ("Qqpr"              , qqpRegister)
        , ("Mod_imm"           , imm12)
        , ("Addr_offset_none"  , gpRegister)
        , ("Addrmode3"         , addrMode3)
        , ("Pred"              , predOperand)
        , ("Cc_out"            , sBitOperand)
        , ("So_reg_imm"        , imm12)
        , ("So_reg_reg"        , imm12)
        , ("Adrlabel"          , adrLabelOperand)
        , ("Arm_bl_target"     , branchTarget)
        , ("Arm_blx_target"    , branchExecuteTarget)
        , ("Arm_br_target"     , branchTarget)
        , ("P_imm"             , coprocRegister)
        , ("Imm0_65535"        , imm16)
        , ("Imm0_15"           , opcodeOperand)
        , ("Imm0_7"            , opcodeOperand)
        , ("Imm0_31"           , imm5)
        , ("C_imm"             , coprocRegister)
        , ("Imod_op"           , word8Operand)
        , ("Iflags_op"         , word8Operand)
        , ("Memb_opt"          , word8Operand)
        , ("Pkh_lsl_amt"       , word8Operand)
        , ("Pkh_asr_amt"       , word8Operand)
        , ("Addrmode_imm12"    , addrModeImm12)
        , ("Ldst_so_reg"       , ldstSoRegOperand)
        , ("Imm1_32"           , imm5)
        , ("Setend_op"         , bit)
        , ("Imm0_1"            , bit)
        , ("Addrmode5"         , addrMode5)
        , ("Addrmode5_pre"     , addrMode5)
        , ("Coproc_option_imm" , word8Operand)
        , ("Postidx_imm8s4"    , word8Operand)
        , ("Reglist"           , word16Operand)
        , ("Imm24b"            , word24Operand)
        , ("Rot_imm"           , word8Operand)
        , ("Shift_imm"         , shiftImm)
        , ("Bf_inv_mask_imm"   , word16Operand)
        , ("Am2offset_imm"     , am2OffsetImm)
        -- , ("Addrmode3_pre"     , )
        -- , ("Addrmode6"         , )
        -- , ("Addrmode_imm12_pre", )
        -- , ("Am2offset_reg"     , )
        -- , ("Am3offset"         , )
        -- , ("Am6offset"         , )
        -- , ("Nohash_imm"        , )
        -- , ("Postidx_imm8"      , )
        -- , ("Postidx_reg"       , )
        -- , ("Rgpr"              , )
        ]

    armFilter = hasNamedString "Namespace" "ARM" &&&
                hasNamedString "DecoderNamespace" "ARM" &&&
                (not . isPseudo)

    armPseudo i = idPseudo i ||
                  idMnemonic i `elem` pseudoInstructionNames

    pseudoInstructionNames =
        [
        ]

    overrides =
        [ ("AES2Op",     FormOverride [("src", Ignore)])
        , ("BFC",        FormOverride [("src", Ignore)])
        , ("BFI",        FormOverride [("src", Ignore)])
        , ("XDB_UPD",    FormOverride [("wb", Ignore)])
        , ("N3SHA3Op",   FormOverride [("src", Ignore)])
        ]
