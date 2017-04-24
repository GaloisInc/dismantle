{-# LANGUAGE TemplateHaskell #-}
module Dismantle.ARM.ISA (
  isa
  ) where

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as LBS
import Data.Int ( Int64 )
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
    addrOffsetNone = OperandPayload { opTypeName = ''ARM.AddrOffsetNone
                                    , opConName = Just 'ARM.AddrOffsetNone
                                    , opConE = Just (conE 'ARM.mkAddrOffsetNone)
                                    , opWordE = Just (varE 'ARM.addrOffsetNoneToBits)
                                    }
    imm12 = OperandPayload { opTypeName = ''ARM.Imm12
                           , opConName = Just 'ARM.Imm12
                           , opConE = Just (conE 'ARM.mkImm12)
                           , opWordE = Just (varE 'ARM.imm12ToBits)
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

    armOperandPayloadTypes =
        [ ("Dpr"               , dpRegister)
        , ("GPR"               , gpRegister)
        , ("Gprpairop"         , gpRegister)
        , ("GPRnopc"           , gpRegister)
        , ("Qpr"               , qpRegister)
        , ("Qqpr"              , qqpRegister)
        , ("Mod_imm"           , imm12)
        , ("Addr_offset_none"  , addrOffsetNone)
        , ("Addrmode3"         , addrMode3)
        , ("Pred"              , predOperand)
        , ("Cc_out"            , sBitOperand)
        -- , ("Addrmode3_pre"     , )
        -- , ("Addrmode5"         , )
        -- , ("Addrmode5_pre"     , )
        -- , ("Addrmode6"         , )
        -- , ("Addrmode_imm12"    , )
        -- , ("Addrmode_imm12_pre", )
        -- , ("Adrlabel"          , )
        -- , ("Am2offset_imm"     , )
        -- , ("Am2offset_reg"     , )
        -- , ("Am3offset"         , )
        -- , ("Am6offset"         , )
        -- , ("Arm_bl_target"     , )
        -- , ("Arm_blx_target"    , )
        -- , ("Arm_br_target"     , )
        -- , ("Bf_inv_mask_imm"   , )
        -- , ("C_imm"             , )
        -- , ("Coproc_option_imm" , )
        -- , ("Iflags_op"         , )
        -- , ("Imm0_1"            , )
        -- , ("Imm0_15"           , )
        -- , ("Imm0_31"           , )
        -- , ("Imm0_65535"        , )
        -- , ("Imm0_7"            , )
        -- , ("Imm1_32"           , )
        -- , ("Imm24b"            , )
        -- , ("Imod_op"           , )
        -- , ("Ldst_so_reg"       , )
        -- , ("Memb_opt"          , )
        -- , ("Nohash_imm"        , )
        -- , ("P_imm"             , )
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
        -- , ("So_reg_imm"        , )
        -- , ("So_reg_reg"        , )
        ]

    armFilter i = and [ idNamespace i == "ARM"
                      , idDecoderNamespace i == "ARM"
                      ]
    armPseudo i = idPseudo i ||
                  idMnemonic i `elem` pseudoInstructionNames

    pseudoInstructionNames =
        [
        ]
