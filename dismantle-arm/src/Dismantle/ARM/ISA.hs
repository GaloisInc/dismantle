{-# LANGUAGE TemplateHaskell #-}
module Dismantle.ARM.ISA (
  isa
  ) where

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import Data.Word ( Word8, Word32, Word64 )

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
          }
  where
    gpRegister = OperandPayload { opTypeName = ''ARM.GPR
                                , opConName = Just 'ARM.GPR
                                , opConE = Just (conE 'ARM.GPR)
                                , opWordE = Just [| fromIntegral . ARM.unGPR |]
                                }
    qpRegister = OperandPayload { opTypeName = ''ARM.QPR
                                , opConName = Just 'ARM.QPR
                                , opConE = Just (conE 'ARM.QPR)
                                , opWordE = Just [| fromIntegral . ARM.unQPR |]
                                }
    qqpRegister = OperandPayload { opTypeName = ''ARM.QQPR
                                 , opConName = Just 'ARM.QQPR
                                 , opConE = Just (conE 'ARM.QQPR)
                                 , opWordE = Just [| fromIntegral . ARM.unQQPR |]
                                 }
    dpRegister = OperandPayload { opTypeName = ''ARM.DPR
                                , opConName = Just 'ARM.DPR
                                , opConE = Just (conE 'ARM.DPR)
                                , opWordE = Just [| fromIntegral . ARM.unDPR |]
                                }
    addrMode3 = OperandPayload { opTypeName = ''ARM.AddrMode3
                               , opConName = Just 'ARM.AddrMode3
                               , opConE = Just (conE 'ARM.mkAddrMode3)
                               , opWordE = Just (varE 'ARM.addrMode3ToBits)
                               }
    imm12 = OperandPayload { opTypeName = ''ARM.Imm12
                           , opConName = Just 'ARM.Imm12
                           , opConE = Just (conE 'ARM.mkImm12)
                           , opWordE = Just (varE 'ARM.imm12ToBits)
                           }
    imm5 = OperandPayload { opTypeName = ''ARM.Imm5
                          , opConName = Just 'ARM.Imm5
                          , opConE = Just (conE 'ARM.mkImm5)
                          , opWordE = Just (varE 'ARM.imm5ToBits)
                          }
    imm12_4Operand = OperandPayload { opTypeName = ''ARM.Imm12_4
                                    , opConName = Just 'ARM.Imm12_4
                                    , opConE = Just (conE 'ARM.mkImm12_4)
                                    , opWordE = Just (varE 'ARM.imm12_4ToBits)
                                    }
    predOperand = OperandPayload { opTypeName = ''ARM.Pred
                                 , opConName = Just 'ARM.Pred
                                 , opConE = Just (conE 'ARM.mkPred)
                                 , opWordE = Just (varE 'ARM.predToBits)
                                 }
    sBitOperand = OperandPayload { opTypeName = ''ARM.SBit
                                 , opConName = Just 'ARM.SBit
                                 , opConE = Just (conE 'ARM.mkSBit)
                                 , opWordE = Just (varE 'ARM.sBitToBits)
                                 }
    adrLabelOperand = OperandPayload { opTypeName = ''ARM.AdrLabel
                                     , opConName = Just 'ARM.AdrLabel
                                     , opConE = Just (conE 'ARM.mkAdrLabel)
                                     , opWordE = Just (varE 'ARM.adrLabelToBits)
                                     }
    branchTarget = OperandPayload { opTypeName = ''ARM.BranchTarget
                                  , opConName = Just 'ARM.BranchTarget
                                  , opConE = Just (conE 'ARM.mkBranchTarget)
                                  , opWordE = Just (varE 'ARM.branchTargetToBits)
                                  }
    branchExecuteTarget = OperandPayload { opTypeName = ''ARM.BranchExecuteTarget
                                         , opConName = Just 'ARM.BranchExecuteTarget
                                         , opConE = Just (conE 'ARM.mkBranchExecuteTarget)
                                         , opWordE = Just (varE 'ARM.branchExecuteTargetToBits)
                                         }
    coprocRegister = OperandPayload { opTypeName = ''ARM.CoprocRegister
                                    , opConName = Just 'ARM.CoprocRegister
                                    , opConE = Just (conE 'ARM.mkCoprocRegister)
                                    , opWordE = Just (varE 'ARM.coprocRegisterToBits)
                                    }
    opcodeOperand = OperandPayload { opTypeName = ''ARM.Opcode
                                   , opConName = Just 'ARM.Opcode
                                   , opConE = Just (conE 'ARM.mkOpcode)
                                   , opWordE = Just (varE 'ARM.opcodeToBits)
                                   }
    word8Operand = OperandPayload { opTypeName = ''Word8
                                  , opConName = Just 'fromIntegral
                                  , opConE = Just (conE 'fromIntegral)
                                  , opWordE = Just (varE 'fromIntegral)
                                  }

    armOperandPayloadTypes =
        [ ("Dpr"               , dpRegister)
        , ("GPR"               , gpRegister)
        , ("Gprpairop"         , gpRegister)
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
        , ("Imm0_65535"        , imm12_4Operand)
        , ("Imm0_15"           , opcodeOperand)
        , ("Imm0_7"            , opcodeOperand)
        , ("Imm0_31"           , imm5)
        , ("C_imm"             , coprocRegister)
        , ("Imod_op"           , word8Operand)
        -- , ("Addrmode3_pre"     , )
        -- , ("Addrmode5"         , )
        -- , ("Addrmode5_pre"     , )
        -- , ("Addrmode6"         , )
        -- , ("Addrmode_imm12"    , )
        -- , ("Addrmode_imm12_pre", )
        -- , ("Am2offset_imm"     , )
        -- , ("Am2offset_reg"     , )
        -- , ("Am3offset"         , )
        -- , ("Am6offset"         , )
        -- , ("Bf_inv_mask_imm"   , )
        -- , ("Coproc_option_imm" , )
        -- , ("Iflags_op"         , )
        -- , ("Imm0_1"            , )
        -- , ("Imm1_32"           , )
        -- , ("Imm24b"            , )
        -- , ("Ldst_so_reg"       , )
        -- , ("Memb_opt"          , )
        -- , ("Nohash_imm"        , )
        -- , ("Pkh_asr_amt"       , )
        -- , ("Pkh_lsl_amt"       , )
        -- , ("Postidx_imm8"      , )
        -- , ("Postidx_imm8s4"    , )
        -- , ("Postidx_reg"       , )
        -- , ("Rgpr"              , )
        -- , ("Reglist"           , )
        -- , ("Rot_imm"           , )
        -- , ("Setend_op"         , )
        -- , ("Shift_imm"         , )
        ]

    armFilter i = and [ idNamespace i == "ARM"
                      , idDecoderNamespace i == "ARM"
                      ]
    armPseudo i = idPseudo i ||
                  idMnemonic i `elem` pseudoInstructionNames

    pseudoInstructionNames =
        [
        ]
