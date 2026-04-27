{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.Thumb.ISA (
  isa
  ) where

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as S
import qualified Data.List.Split as L
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Word ( Word8, Word16, Word32 )

import Language.Haskell.TH

import Dismantle.Tablegen.ISA
import Dismantle.Tablegen.Types
import Dismantle.Tablegen.Parser.Types
  ( Metadata(Metadata)
  , defName
  , defMetadata
  )
import qualified Dismantle.Thumb.Operands as Thumb
import qualified Dismantle.ARM.Operands as ARM

fullWordStartPatterns :: [Word16]
fullWordStartPatterns =
    [ 0b11101 `shiftL` 11
    , 0b11110 `shiftL` 11
    , 0b11111 `shiftL` 11
    ]

asWord32 :: LBS.ByteString -> Word32
asWord32 = B.runGet $ do
    w1 <- B.getWord16le
    -- These bit patterns indicate a full word instruction so we need to
    -- parse another halfword and shift the first word left.
    --
    -- For details, see the ARM ARM A6.1 Thumb Instruction Set Encoding,
    -- version ARM DDI 0406C.b.
    let matchesPattern p = (w1 .&. p) == p
        fullWord = any matchesPattern fullWordStartPatterns
    if not fullWord
       then return $ fromIntegral w1
       else do
           w2 <- B.getWord16le
           return $ (((fromIntegral w1)::Word32) `shiftL` 16) .|.
                    (fromIntegral w2)

fromWord32 :: Word32 -> LBS.ByteString
fromWord32 w =
    let matchesPattern p =
            let p' = ((fromIntegral p) :: Word32) `shiftL` 16
            in (w .&. p') == p'
        fullWord = any matchesPattern fullWordStartPatterns
        halfWordMask = 0xffff
    in if fullWord
       then B.runPut $ do
           B.putWord16le (fromIntegral $ w `shiftR` 16)
           B.putWord16le (fromIntegral $ w .&. halfWordMask)
       else B.runPut $ B.putWord16le (fromIntegral $ w .&. halfWordMask)

endianness :: Endianness
endianness =
    let rewriteBytes = LBS.pack . concat . fmap reverse . L.chunksOf 2 . LBS.unpack
        rewriteMask = concat . fmap (concat . reverse . L.chunksOf 8) . L.chunksOf 16
    in Little rewriteBytes rewriteMask

isa :: ISA
isa = ISA { isaName = "Thumb"
          , isaTgenBitPreprocess = id
          , isaInputEndianness = endianness
          , isaInstructionFilter = thumbFilter
          , isaPseudoInstruction = thumbPseudo
          , isaOperandPayloadTypes = thumbOperandPayloadTypes
          , isaInsnWordFromBytes = 'asWord32
          , isaInsnWordToBytes = 'fromWord32
          , isaInsnAssembleType = ''Word32
          , isaIgnoreOperand = const False
          , isaFormOverrides = overrides
          , isaMapOperandPayloadType = id
          , isaUnusedBitsPolicy = Just Drop
          , isaDefaultPrettyVariableValues = defaultPP
          , isaPrettyOverrides = prettyOverrides
          }
  where
    defaultPP = [ ("p", "")
                , ("Rdn", "")
                , ("s", "s")
                , ("sp", "sp")
                ]

    gpRegister = OperandPayload { opTypeT = [t| Thumb.GPR |]
                                , opConE = Just (varE 'Thumb.gpr)
                                , opWordE = Just [| fromIntegral . Thumb.unGPR |]
                                }
    lowGpRegister = OperandPayload { opTypeT = [t| Thumb.LowGPR |]
                                   , opConE = Just (varE 'Thumb.lowGpr)
                                   , opWordE = Just [| fromIntegral . Thumb.unLowGPR |]
                                   }
    addrMode5 = OperandPayload { opTypeT = [t| ARM.AddrMode5 |]
                               , opConE = Just (varE 'ARM.mkAddrMode5)
                               , opWordE = Just (varE 'ARM.addrMode5ToBits)
                               }
    bankedReg = OperandPayload { opTypeT = [t| ARM.BankedReg |]
                               , opConE = Just (varE 'ARM.mkBankedReg)
                               , opWordE = Just (varE 'ARM.bankedRegToBits)
                               }
    t2SoReg = OperandPayload { opTypeT = [t| Thumb.T2SoReg |]
                             , opConE = Just (varE 'Thumb.mkT2SoReg)
                             , opWordE = Just (varE 'Thumb.t2SoRegToBits)
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
    addrModePc = OperandPayload { opTypeT = [t| Thumb.AddrModePc |]
                                , opConE = Just (varE 'Thumb.mkAddrModePc)
                                , opWordE = Just (varE 'Thumb.addrModePcToBits)
                                }
    addrModeSp = OperandPayload { opTypeT = [t| Thumb.AddrModeSp |]
                                , opConE = Just (varE 'Thumb.mkAddrModeSp)
                                , opWordE = Just (varE 'Thumb.addrModeSpToBits)
                                }
    tImm0508s4 = OperandPayload { opTypeT = [t| Thumb.TImm0508S4 |]
                                , opConE = Just (varE 'Thumb.mkTImm0508S4)
                                , opWordE = Just (varE 'Thumb.tImm0508S4ToBits)
                                }
    tImm01020s4 = OperandPayload { opTypeT = [t| Thumb.TImm01020S4 |]
                                 , opConE = Just (varE 'Thumb.mkTImm01020S4)
                                 , opWordE = Just (varE 'Thumb.tImm01020S4ToBits)
                                 }
    addrModeRr = OperandPayload { opTypeT = [t| Thumb.AddrModeRr |]
                                , opConE = Just (varE 'Thumb.mkAddrModeRr)
                                , opWordE = Just (varE 'Thumb.addrModeRrToBits)
                                }
    addrModeImm01020S4 = OperandPayload { opTypeT = [t| Thumb.AddrModeImm01020S4 |]
                                        , opConE = Just (varE 'Thumb.mkAddrModeImm01020S4)
                                        , opWordE = Just (varE 'Thumb.addrModeImm01020S4ToBits)
                                        }
    t2AddrModeImm12 = OperandPayload { opTypeT = [t| Thumb.T2AddrModeImm12 |]
                                     , opConE = Just (varE 'Thumb.mkT2AddrModeImm12)
                                     , opWordE = Just (varE 'Thumb.t2AddrModeImm12ToBits)
                                     }
    t2AddrModeSoReg = OperandPayload { opTypeT = [t| Thumb.T2AddrModeSoReg |]
                                     , opConE = Just (varE 'Thumb.mkT2AddrModeSoReg)
                                     , opWordE = Just (varE 'Thumb.t2AddrModeSoRegToBits)
                                     }
    t2AddrModeImm8 = OperandPayload { opTypeT = [t| Thumb.T2AddrModeImm8 |]
                                    , opConE = Just (varE 'Thumb.mkT2AddrModeImm8)
                                    , opWordE = Just (varE 'Thumb.t2AddrModeImm8ToBits)
                                    }
    t2AddrModeImm8Offset = OperandPayload { opTypeT = [t| Thumb.T2AddrModeImm8Offset |]
                                          , opConE = Just (varE 'Thumb.mkT2AddrModeImm8Offset)
                                          , opWordE = Just (varE 'Thumb.t2AddrModeImm8OffsetToBits)
                                          }
    t2AddrModeImm8S4Offset = OperandPayload { opTypeT = [t| Thumb.T2AddrModeImm8S4Offset |]
                                            , opConE = Just (varE 'Thumb.mkT2AddrModeImm8S4Offset)
                                            , opWordE = Just (varE 'Thumb.t2AddrModeImm8S4OffsetToBits)
                                            }
    t2AddrModeNegImm8 = OperandPayload { opTypeT = [t| Thumb.T2AddrModeNegImm8 |]
                                       , opConE = Just (varE 'Thumb.mkT2AddrModeNegImm8)
                                       , opWordE = Just (varE 'Thumb.t2AddrModeNegImm8ToBits)
                                       }
    t2AddrModeImm8S4 = OperandPayload { opTypeT = [t| Thumb.T2AddrModeImm8S4 |]
                                      , opConE = Just (varE 'Thumb.mkT2AddrModeImm8S4)
                                      , opWordE = Just (varE 'Thumb.t2AddrModeImm8S4ToBits)
                                      }
    t2AddrModeImm8S4Pre = OperandPayload { opTypeT = [t| Thumb.T2AddrModeImm8S4Pre |]
                                         , opConE = Just (varE 'Thumb.mkT2AddrModeImm8S4Pre)
                                         , opWordE = Just (varE 'Thumb.t2AddrModeImm8S4PreToBits)
                                         }
    t2AddrModeImm8Pre = OperandPayload { opTypeT = [t| Thumb.T2AddrModeImm8Pre |]
                                       , opConE = Just (varE 'Thumb.mkT2AddrModeImm8Pre)
                                       , opWordE = Just (varE 'Thumb.t2AddrModeImm8PreToBits)
                                       }
    predOperand = OperandPayload { opTypeT = [t| Thumb.Pred |]
                                 , opConE = Just (varE 'Thumb.mkPred)
                                 , opWordE = Just (varE 'Thumb.predToBits)
                                 }
    tAdrLabelOperand = OperandPayload { opTypeT = [t| Thumb.TAdrLabel |]
                                      , opConE = Just (varE 'Thumb.mkTAdrLabel)
                                      , opWordE = Just (varE 'Thumb.tAdrLabelToBits)
                                      }
    t2AdrLabelOperand = OperandPayload { opTypeT = [t| Thumb.T2AdrLabel |]
                                       , opConE = Just (varE 'Thumb.mkT2AdrLabel)
                                       , opWordE = Just (varE 'Thumb.t2AdrLabelToBits)
                                       }
    t2LdrLabelOperand = OperandPayload { opTypeT = [t| Thumb.T2LdrLabel |]
                                       , opConE = Just (varE 'Thumb.mkT2LdrLabel)
                                       , opWordE = Just (varE 'Thumb.t2LdrLabelToBits)
                                       }
    tBrTargetOperand = OperandPayload { opTypeT = [t| Thumb.TBrTarget |]
                                      , opConE = Just (varE 'Thumb.mkTBrTarget)
                                      , opWordE = Just (varE 'Thumb.tBrTargetToBits)
                                      }
    thumbBlTarget = OperandPayload { opTypeT = [t| Thumb.ThumbBlTarget |]
                                   , opConE = Just (varE 'Thumb.mkThumbBlTarget)
                                   , opWordE = Just (varE 'Thumb.thumbBlTargetToBits)
                                   }
    thumbBlxTarget = OperandPayload { opTypeT = [t| Thumb.ThumbBlxTarget |]
                                    , opConE = Just (varE 'Thumb.mkThumbBlxTarget)
                                    , opWordE = Just (varE 'Thumb.thumbBlxTargetToBits)
                                    }
    opcodeOperand = OperandPayload { opTypeT = [t| Thumb.Opcode |]
                                   , opConE = Just (varE 'Thumb.mkOpcode)
                                   , opWordE = Just (varE 'Thumb.opcodeToBits)
                                   }
    rotImm = OperandPayload { opTypeT = [t| Thumb.RotImm |]
                            , opConE = Just (varE 'Thumb.mkRotImm)
                            , opWordE = Just (varE 'Thumb.rotImmToBits)
                            }
    word8Operand = OperandPayload { opTypeT = [t| Word8 |]
                                  , opConE = Nothing
                                  , opWordE = Just [| fromIntegral |]
                                  }
    imm065535 = OperandPayload { opTypeT = [t| Thumb.Imm065535 |]
                               , opConE = Just (varE 'Thumb.mkImm065535)
                               , opWordE = Just (varE 'Thumb.imm065535ToBits)
                               }
    word16Operand = OperandPayload { opTypeT = [t| Word16 |]
                                   , opConE = Nothing
                                   , opWordE = Just [| fromIntegral |]
                                   }
    reglistOperand = OperandPayload { opTypeT = [t| Thumb.Reglist |]
                                    , opConE = Just (varE 'Thumb.mkRegList)
                                    , opWordE = Just (varE 'Thumb.regListToBits)
                                    }
    word32Operand = OperandPayload { opTypeT = [t| Word32 |]
                                   , opConE = Nothing
                                   , opWordE = Nothing
                                   }
    coprocRegister = OperandPayload { opTypeT = [t| ARM.CoprocRegister |]
                                    , opConE = Just (varE 'ARM.mkCoprocRegister)
                                    , opWordE = Just (varE 'ARM.coprocRegisterToBits)
                                    }
    bit = OperandPayload { opTypeT = [t| Thumb.Bit |]
                         , opConE = Just (varE 'Thumb.mkBit)
                         , opWordE = Just (varE 'Thumb.bitToBits)
                         }
    t2SoImmOperand = OperandPayload { opTypeT = [t| Thumb.T2SoImm |]
                                    , opConE = Just (varE 'Thumb.mkT2SoImm)
                                    , opWordE = Just (varE 'Thumb.t2SoImmToBits)
                                    }
    sBit = OperandPayload { opTypeT = [t| ARM.SBit |]
                          , opConE = Just (varE 'ARM.mkSBit)
                          , opWordE = Just (varE 'ARM.sBitToBits)
                          }
    imm5 = OperandPayload { opTypeT = [t| ARM.Imm5 |]
                          , opConE = Just (varE 'ARM.mkImm5)
                          , opWordE = Just (varE 'ARM.imm5ToBits)
                          }
    imm1_32 = OperandPayload { opTypeT = [t| Thumb.Imm1_32 |]
                             , opConE = Just (varE 'Thumb.mkImm1_32)
                             , opWordE = Just (varE 'Thumb.imm1_32ToBits)
                             }
    msrMask = OperandPayload { opTypeT = [t| ARM.MSRMask |]
                             , opConE = Just (varE 'ARM.mkMSRMask)
                             , opWordE = Just (varE 'ARM.msrMaskToBits)
                             }
    imm8s4 = OperandPayload { opTypeT = [t| ARM.Imm8S4 |]
                            , opConE = Just (varE 'ARM.mkImm8s4)
                            , opWordE = Just (varE 'ARM.imm8s4ToBits)
                            }
    bfInvMaskImm = OperandPayload { opTypeT = [t| Thumb.BfInvMaskImm |]
                                  , opConE = Just (varE 'Thumb.mkBfInvMaskImm)
                                  , opWordE = Just (varE 'Thumb.bfInvMaskImmToBits)
                                  }

    thumbOperandPayloadTypes =
        [ ("GPR"                  , gpRegister)
        , ("RGPR"                 , gpRegister)
        , ("GPRwithAPSR"          , gpRegister)
        , ("Addr_offset_none"     , gpRegister)
        , ("Addrmode5"            , addrMode5)
        , ("Addrmode5_pre"        , addrMode5)
        , ("Bf_inv_mask_imm"      , bfInvMaskImm)
        , ("Banked_reg"           , bankedReg)
        , ("TGPR"                 , lowGpRegister)
        , ("T_addrmode_is1"       , addrModeIs1)
        , ("T_addrmode_is2"       , addrModeIs2)
        , ("T_addrmode_is4"       , addrModeIs4)
        , ("T_addrmode_pc"        , addrModePc)
        , ("T_addrmode_rr"        , addrModeRr)
        , ("T_addrmode_sp"        , addrModeSp)
        , ("T_adrlabel"           , tAdrLabelOperand)
        , ("T2adrlabel"           , t2AdrLabelOperand)
        , ("T2ldrlabel"           , t2LdrLabelOperand)
        , ("T_brtarget"           , tBrTargetOperand)
        , ("T_imm0_1020s4"        , tImm01020s4)
        , ("T_imm0_508s4"         , tImm0508s4)
        , ("T2addrmode_imm0_1020s4", addrModeImm01020S4)
        , ("T2addrmode_imm12"     , t2AddrModeImm12)
        , ("T2addrmode_imm8"      , t2AddrModeImm8)
        , ("T2am_imm8_offset"     , t2AddrModeImm8Offset)
        , ("T2am_imm8s4_offset"   , t2AddrModeImm8S4Offset)
        , ("T2addrmode_so_reg"    , t2AddrModeSoReg)
        , ("T2addrmode_posimm8"   , t2AddrModeImm8)
        , ("T2addrmode_negimm8"   , t2AddrModeNegImm8)
        , ("T2addrmode_imm8s4"    , t2AddrModeImm8S4)
        , ("T2addrmode_imm8s4_pre", t2AddrModeImm8S4Pre)
        , ("T2addrmode_imm8_pre"  , t2AddrModeImm8Pre)
        , ("Thumb_bl_target"      , thumbBlTarget)
        , ("Thumb_br_target"      , thumbBlTarget)
        , ("Thumb_blx_target"     , thumbBlxTarget)
        , ("Thumb_bcc_target"     , word8Operand)
        , ("Thumb_cb_target"      , word8Operand)
        , ("GPRnopc"              , gpRegister)
        , ("Iflags_op"            , word8Operand)
        , ("Imm0_4095"            , word16Operand)
        , ("Imm0_65535"           , imm065535)
        , ("Imm0_65535_expr"      , imm065535)
        , ("Imm0_1"               , bit)
        , ("Imm0_7"               , opcodeOperand)
        , ("Imm0_15"              , opcodeOperand)
        , ("Imm0_31"              , imm5)
        , ("Imm0_63"              , word8Operand)
        , ("Imm0_239"             , word8Operand)
        , ("Imm0_255"             , word8Operand)
        , ("Imm1_16"              , word8Operand)
        , ("Imm1_32"              , imm1_32)
        , ("Imm_sr"               , word8Operand)
        , ("Imod_op"              , word8Operand)
        , ("Instsyncb_opt"        , Thumb.membOptOperand)
        , ("It_mask"              , word8Operand)
        , ("It_pred"              , predOperand)
        , ("Memb_opt"             , Thumb.membOptOperand)
        , ("Msr_mask"             , msrMask)
        , ("Msr_mask_10"          , Thumb.msrMask10Operand)
        , ("P_imm"                , word8Operand)
        , ("Pkh_asr_amt"          , word8Operand)
        , ("Pkh_lsl_amt"          , word8Operand)
        , ("Rot_imm"              , rotImm)
        , ("T2_shift_imm"         , word8Operand)
        , ("T2_so_imm"            , t2SoImmOperand)
        , ("Postidx_imm8s4"       , imm8s4)
        , ("Pred"                 , predOperand)
        , ("Reglist"              , reglistOperand)
        , ("Setend_op"            , bit)
        , ("Unpredictable"        , word32Operand)
        , ("Brtarget"             , word32Operand)
        , ("C_imm"                , coprocRegister)
        , ("Cc_out"               , sBit)
        , ("Coproc_option_imm"    , word8Operand)
        , ("I32imm"               , word8Operand)
        , ("T2_so_reg"            , t2SoReg)
        ]

    thumbFilter = hasNamedString "Namespace" "ARM" &&&
                  (hasNamedString "DecoderNamespace" "Thumb" |||
                   hasNamedString "DecoderNamespace" "ThumbSBit" |||
                   hasNamedString "DecoderNamespace" "Thumb2") &&&
                  (not . isPseudo) &&&
                  (not . ignoredDef) &&&
                  (not . ignoredMetadata)

    thumbPseudo = idPseudo

    ignoredDef d = defName d `elem`
        [ "tADDrSP" -- See tADDhirr
        , "tADDspr" -- See tADDhirr
        , "t2LDRBpci" -- See t2LDRB_POST
        , "t2LDRHpci" -- See t2LDRH_POST
        , "t2LDRpci" -- See t2LDR_POST
        , "t2LDRSBpci" -- See t2LDRSB_POST
        , "t2LDRSHpci" -- See t2LDRSH_POST
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
        [ ("t2ADCri",           FormOverride [("p", Ignore)])
        , ("t2ADCrr",           FormOverride [("p", Ignore)])
        , ("t2ADCrs",           FormOverride [("p", Ignore)])
        , ("t2ADDri",           FormOverride [("p", Ignore)])
        , ("t2ADDri12",         FormOverride [("p", Ignore)])
        , ("t2ADDrr",           FormOverride [("p", Ignore)])
        , ("t2ADDrs",           FormOverride [("p", Ignore)])
        , ("t2ADR",             FormOverride [("p", Ignore)])
        , ("t2ANDri",           FormOverride [("p", Ignore)])
        , ("t2ANDrr",           FormOverride [("p", Ignore)])
        , ("t2ANDrs",           FormOverride [("p", Ignore)])
        , ("t2ASRri",           FormOverride [("p", Ignore)])
        , ("t2ASRrr",           FormOverride [("p", Ignore)])
        , ("t2B",               FormOverride [("p", Ignore)])
        , ("t2BFC",             FormOverride [("src", Ignore), ("p", Ignore)])
        , ("t2BFI",             FormOverride [("src", Ignore), ("p", Ignore)])
        , ("t2BICri",           FormOverride [("p", Ignore)])
        , ("t2BICrr",           FormOverride [("p", Ignore)])
        , ("t2BICrs",           FormOverride [("p", Ignore)])
        , ("t2BXJ",             FormOverride [("p", Ignore)])
        , ("t2CDP",             FormOverride [("p", Ignore)])
        , ("t2CDP2",            FormOverride [("p", Ignore)])
        , ("t2CLREX",           FormOverride [("p", Ignore)])
        , ("t2CLZ",             FormOverride [("p", Ignore)])
        , ("t2CMNri",           FormOverride [("p", Ignore)])
        , ("t2CMNzrr",          FormOverride [("p", Ignore)])
        , ("t2CMNzrs",          FormOverride [("p", Ignore)])
        , ("t2CMPri",           FormOverride [("p", Ignore)])
        , ("t2CMPrr",           FormOverride [("p", Ignore)])
        , ("t2CMPrs",           FormOverride [("p", Ignore)])
        , ("t2DBG",             FormOverride [("p", Ignore)])
        , ("t2DCPS1",           FormOverride [("p", Ignore)])
        , ("t2DCPS2",           FormOverride [("p", Ignore)])
        , ("t2DCPS3",           FormOverride [("p", Ignore)])
        , ("t2DMB",             FormOverride [("p", Ignore)])
        , ("t2DSB",             FormOverride [("p", Ignore)])
        , ("t2EORri",           FormOverride [("p", Ignore)])
        , ("t2EORrr",           FormOverride [("p", Ignore)])
        , ("t2EORrs",           FormOverride [("p", Ignore)])
        , ("t2HINT",            FormOverride [("p", Ignore)])
        , ("t2ISB",             FormOverride [("p", Ignore)])
        , ("t2Int_eh_sjlj_setjmp", FormOverride [("src", Ignore), ("val", Ignore)])
        , ("t2Int_eh_sjlj_setjmp_nofp", FormOverride [("src", Ignore), ("val", Ignore)])
        , ("t2LDA",             FormOverride [("p", Ignore)])
        , ("t2LDAB",            FormOverride [("p", Ignore)])
        , ("t2LDAEX",           FormOverride [("p", Ignore)])
        , ("t2LDAEXB",          FormOverride [("p", Ignore)])
        , ("t2LDAEXD",          FormOverride [("p", Ignore)])
        , ("t2LDAEXH",          FormOverride [("p", Ignore)])
        , ("t2LDAH",            FormOverride [("p", Ignore)])
        , ("t2LDC2L_OFFSET",    FormOverride [("p", Ignore)])
        , ("t2LDC2L_OPTION",    FormOverride [("p", Ignore)])
        , ("t2LDC2L_POST",      FormOverride [("p", Ignore)])
        , ("t2LDC2L_PRE",       FormOverride [("p", Ignore)])
        , ("t2LDC2_OFFSET",     FormOverride [("p", Ignore)])
        , ("t2LDC2_OPTION",     FormOverride [("p", Ignore)])
        , ("t2LDC2_POST",       FormOverride [("p", Ignore)])
        , ("t2LDC2_PRE",        FormOverride [("p", Ignore)])
        , ("t2LDCL_OFFSET",     FormOverride [("p", Ignore)])
        , ("t2LDCL_OPTION",     FormOverride [("p", Ignore)])
        , ("t2LDCL_POST",       FormOverride [("p", Ignore)])
        , ("t2LDCL_PRE",        FormOverride [("p", Ignore)])
        , ("t2LDC_OFFSET",      FormOverride [("p", Ignore)])
        , ("t2LDC_OPTION",      FormOverride [("p", Ignore)])
        , ("t2LDC_POST",        FormOverride [("p", Ignore)])
        , ("t2LDC_PRE",         FormOverride [("p", Ignore)])
        , ("t2LDMDB",           FormOverride [("p", Ignore)])
        , ("t2LDMDB_UPD",       FormOverride [("wb", Ignore), ("p", Ignore)])
        , ("t2LDMIA",           FormOverride [("p", Ignore)])
        , ("t2LDMIA_UPD",       FormOverride [("wb", Ignore), ("p", Ignore)])
        , ("t2LDRBT",           FormOverride [("p", Ignore)])
        , ("t2LDRB_POST",       FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2LDRB_PRE",        FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2LDRBi12",         FormOverride [("p", Ignore)])
        , ("t2LDRBi8",          FormOverride [("p", Ignore)])
        , ("t2LDRBpci",         FormOverride [("p", Ignore)])
        , ("t2LDRBs",           FormOverride [("p", Ignore)])
        , ("t2LDRD_POST",       FormOverride [("wb", Ignore), ("p", Ignore)])
        , ("t2LDRD_PRE",        FormOverride [("wb", Ignore), ("p", Ignore)])
        , ("t2LDRDi8",          FormOverride [("p", Ignore)])
        , ("t2LDREX",           FormOverride [("p", Ignore)])
        , ("t2LDREXB",          FormOverride [("p", Ignore)])
        , ("t2LDREXD",          FormOverride [("p", Ignore)])
        , ("t2LDREXH",          FormOverride [("p", Ignore)])
        , ("t2LDRHT",           FormOverride [("p", Ignore)])
        , ("t2LDRH_POST",       FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2LDRH_PRE",        FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2LDRHi12",         FormOverride [("p", Ignore)])
        , ("t2LDRHi8",          FormOverride [("p", Ignore)])
        , ("t2LDRHpci",         FormOverride [("p", Ignore)])
        , ("t2LDRHs",           FormOverride [("p", Ignore)])
        , ("t2LDRSBT",          FormOverride [("p", Ignore)])
        , ("t2LDRSB_POST",      FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2LDRSB_PRE",       FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2LDRSBi12",        FormOverride [("p", Ignore)])
        , ("t2LDRSBi8",         FormOverride [("p", Ignore)])
        , ("t2LDRSBpci",        FormOverride [("p", Ignore)])
        , ("t2LDRSBs",          FormOverride [("p", Ignore)])
        , ("t2LDRSHT",          FormOverride [("p", Ignore)])
        , ("t2LDRSH_POST",      FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2LDRSH_PRE",       FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2LDRSHi12",        FormOverride [("p", Ignore)])
        , ("t2LDRSHi8",         FormOverride [("p", Ignore)])
        , ("t2LDRSHpci",        FormOverride [("p", Ignore)])
        , ("t2LDRSHs",          FormOverride [("p", Ignore)])
        , ("t2LDRT",            FormOverride [("p", Ignore)])
        , ("t2LDR_POST",        FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2LDR_PRE",         FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2LDRi12",          FormOverride [("p", Ignore)])
        , ("t2LDRi8",           FormOverride [("p", Ignore)])
        , ("t2LDRpci",          FormOverride [("p", Ignore)])
        , ("t2LDRs",            FormOverride [("p", Ignore)])
        , ("tLSLri",            FormOverride [("p", Ignore), ("s", Ignore)])
        , ("t2LSLri",           FormOverride [("p", Ignore)])
        , ("tLSLrr",            FormOverride [("p", Ignore), ("s", Ignore), ("Rn", Ignore)])
        , ("t2LSLrr",           FormOverride [("p", Ignore)])
        , ("t2LSRri",           FormOverride [("p", Ignore)])
        , ("t2LSRrr",           FormOverride [("p", Ignore)])
        , ("t2MCR",             FormOverride [("p", Ignore)])
        , ("t2MCR2",            FormOverride [("p", Ignore)])
        , ("t2MCRR",            FormOverride [("p", Ignore)])
        , ("t2MCRR2",           FormOverride [("p", Ignore)])
        , ("t2MLA",             FormOverride [("p", Ignore)])
        , ("t2MLS",             FormOverride [("p", Ignore)])
        , ("t2MOVTi16",         FormOverride [("src", Ignore), ("p", Ignore)])
        , ("t2MOVi",            FormOverride [("p", Ignore)])
        , ("t2MOVi16",          FormOverride [("p", Ignore)])
        , ("t2MOVr",            FormOverride [("p", Ignore)])
        , ("t2MOVsra_flag",     FormOverride [("p", Ignore)])
        , ("t2MOVsrl_flag",     FormOverride [("p", Ignore)])
        , ("t2MRC",             FormOverride [("p", Ignore)])
        , ("t2MRC2",            FormOverride [("p", Ignore)])
        , ("t2MRRC",            FormOverride [("p", Ignore)])
        , ("t2MRRC2",           FormOverride [("p", Ignore)])
        , ("t2MRS_AR",          FormOverride [("p", Ignore)])
        , ("t2MRS_M",           FormOverride [("p", Ignore)])
        , ("t2MRSbanked",       FormOverride [("p", Ignore)])
        , ("t2MRSsys_AR",       FormOverride [("p", Ignore)])
        , ("t2MSR_AR",          FormOverride [("p", Ignore)])
        , ("t2MSR_M",           FormOverride [("p", Ignore)])
        , ("t2MSRbanked",       FormOverride [("p", Ignore)])
        , ("t2MUL",             FormOverride [("p", Ignore)])
        , ("t2MVNi",            FormOverride [("p", Ignore)])
        , ("t2MVNr",            FormOverride [("p", Ignore)])
        , ("t2MVNs",            FormOverride [("p", Ignore)])
        , ("t2ORNri",           FormOverride [("p", Ignore)])
        , ("t2ORNrr",           FormOverride [("p", Ignore)])
        , ("t2ORNrs",           FormOverride [("p", Ignore)])
        , ("t2ORRri",           FormOverride [("p", Ignore)])
        , ("t2ORRrr",           FormOverride [("p", Ignore)])
        , ("t2ORRrs",           FormOverride [("p", Ignore)])
        , ("t2PKHBT",           FormOverride [("p", Ignore)])
        , ("t2PKHTB",           FormOverride [("p", Ignore)])
        , ("t2PLDWi12",         FormOverride [("p", Ignore)])
        , ("t2PLDWi8",          FormOverride [("p", Ignore)])
        , ("t2PLDWs",           FormOverride [("p", Ignore)])
        , ("t2PLDi12",          FormOverride [("p", Ignore)])
        , ("t2PLDi8",           FormOverride [("p", Ignore)])
        , ("t2PLDpci",          FormOverride [("p", Ignore)])
        , ("t2PLDs",            FormOverride [("p", Ignore)])
        , ("t2PLIi12",          FormOverride [("p", Ignore)])
        , ("t2PLIi8",           FormOverride [("p", Ignore)])
        , ("t2PLIpci",          FormOverride [("p", Ignore)])
        , ("t2PLIs",            FormOverride [("p", Ignore)])
        , ("t2QADD",            FormOverride [("p", Ignore)])
        , ("t2QADD16",          FormOverride [("p", Ignore)])
        , ("t2QADD8",           FormOverride [("p", Ignore)])
        , ("t2QASX",            FormOverride [("p", Ignore)])
        , ("t2QDADD",           FormOverride [("p", Ignore)])
        , ("t2QDSUB",           FormOverride [("p", Ignore)])
        , ("t2QSAX",            FormOverride [("p", Ignore)])
        , ("t2QSUB",            FormOverride [("p", Ignore)])
        , ("t2QSUB16",          FormOverride [("p", Ignore)])
        , ("t2QSUB8",           FormOverride [("p", Ignore)])
        , ("t2RBIT",            FormOverride [("p", Ignore)])
        , ("t2REV",             FormOverride [("p", Ignore)])
        , ("t2REV16",           FormOverride [("p", Ignore)])
        , ("t2REVSH",           FormOverride [("p", Ignore)])
        , ("t2RFEDB",           FormOverride [("p", Ignore)])
        , ("t2RFEDBW",          FormOverride [("p", Ignore)])
        , ("t2RFEIA",           FormOverride [("p", Ignore)])
        , ("t2RFEIAW",          FormOverride [("p", Ignore)])
        , ("t2RORri",           FormOverride [("p", Ignore)])
        , ("t2RORrr",           FormOverride [("p", Ignore)])
        , ("t2RRX",             FormOverride [("p", Ignore)])
        , ("t2RSBri",           FormOverride [("p", Ignore)])
        , ("t2RSBrr",           FormOverride [("p", Ignore)])
        , ("t2RSBrs",           FormOverride [("p", Ignore)])
        , ("t2SADD16",          FormOverride [("p", Ignore)])
        , ("t2SADD8",           FormOverride [("p", Ignore)])
        , ("t2SASX",            FormOverride [("p", Ignore)])
        , ("t2SBCri",           FormOverride [("p", Ignore)])
        , ("t2SBCrr",           FormOverride [("p", Ignore)])
        , ("t2SBCrs",           FormOverride [("p", Ignore)])
        , ("t2SBFX",            FormOverride [("p", Ignore)])
        , ("t2SDIV",            FormOverride [("p", Ignore)])
        , ("t2SEL",             FormOverride [("p", Ignore)])
        , ("t2SG",              FormOverride [("p", Ignore)])
        , ("t2SHADD16",         FormOverride [("p", Ignore)])
        , ("t2SHADD8",          FormOverride [("p", Ignore)])
        , ("t2SHASX",           FormOverride [("p", Ignore)])
        , ("t2SHSAX",           FormOverride [("p", Ignore)])
        , ("t2SHSUB16",         FormOverride [("p", Ignore)])
        , ("t2SHSUB8",          FormOverride [("p", Ignore)])
        , ("t2SMC",             FormOverride [("p", Ignore)])
        , ("t2SMLABB",          FormOverride [("p", Ignore)])
        , ("t2SMLABT",          FormOverride [("p", Ignore)])
        , ("t2SMLAD",           FormOverride [("p", Ignore)])
        , ("t2SMLADX",          FormOverride [("p", Ignore)])
        , ("t2SMLAL",           FormOverride [("RLo", Ignore), ("RHi", Ignore), ("p", Ignore)])
        , ("t2SMLALBB",         FormOverride [("p", Ignore)])
        , ("t2SMLALBT",         FormOverride [("p", Ignore)])
        , ("t2SMLALD",          FormOverride [("p", Ignore)])
        , ("t2SMLALDX",         FormOverride [("p", Ignore)])
        , ("t2SMLALTB",         FormOverride [("p", Ignore)])
        , ("t2SMLALTT",         FormOverride [("p", Ignore)])
        , ("t2SMLATB",          FormOverride [("p", Ignore)])
        , ("t2SMLATT",          FormOverride [("p", Ignore)])
        , ("t2SMLAWB",          FormOverride [("p", Ignore)])
        , ("t2SMLAWT",          FormOverride [("p", Ignore)])
        , ("t2SMLSD",           FormOverride [("p", Ignore)])
        , ("t2SMLSDX",          FormOverride [("p", Ignore)])
        , ("t2SMLSLD",          FormOverride [("p", Ignore)])
        , ("t2SMLSLDX",         FormOverride [("p", Ignore)])
        , ("t2SMMLA",           FormOverride [("p", Ignore)])
        , ("t2SMMLAR",          FormOverride [("p", Ignore)])
        , ("t2SMMLS",           FormOverride [("p", Ignore)])
        , ("t2SMMLSR",          FormOverride [("p", Ignore)])
        , ("t2SMMUL",           FormOverride [("p", Ignore)])
        , ("t2SMMULR",          FormOverride [("p", Ignore)])
        , ("t2SMUAD",           FormOverride [("p", Ignore)])
        , ("t2SMUADX",          FormOverride [("p", Ignore)])
        , ("t2SMULBB",          FormOverride [("p", Ignore)])
        , ("t2SMULBT",          FormOverride [("p", Ignore)])
        , ("t2SMULL",           FormOverride [("p", Ignore)])
        , ("t2SMULTB",          FormOverride [("p", Ignore)])
        , ("t2SMULTT",          FormOverride [("p", Ignore)])
        , ("t2SMULWB",          FormOverride [("p", Ignore)])
        , ("t2SMULWT",          FormOverride [("p", Ignore)])
        , ("t2SMUSD",           FormOverride [("p", Ignore)])
        , ("t2SMUSDX",          FormOverride [("p", Ignore)])
        , ("t2SRSDB",           FormOverride [("p", Ignore)])
        , ("t2SRSDB_UPD",       FormOverride [("p", Ignore)])
        , ("t2SRSIA",           FormOverride [("p", Ignore)])
        , ("t2SRSIA_UPD",       FormOverride [("p", Ignore)])
        , ("t2SSAT",            FormOverride [("p", Ignore)])
        , ("t2SSAT16",          FormOverride [("p", Ignore)])
        , ("t2SSAX",            FormOverride [("p", Ignore)])
        , ("t2SSUB16",          FormOverride [("p", Ignore)])
        , ("t2SSUB8",           FormOverride [("p", Ignore)])
        , ("t2STC2L_OFFSET",    FormOverride [("p", Ignore)])
        , ("t2STC2L_OPTION",    FormOverride [("p", Ignore)])
        , ("t2STC2L_POST",      FormOverride [("p", Ignore)])
        , ("t2STC2L_PRE",       FormOverride [("p", Ignore)])
        , ("t2STC2_OFFSET",     FormOverride [("p", Ignore)])
        , ("t2STC2_OPTION",     FormOverride [("p", Ignore)])
        , ("t2STC2_POST",       FormOverride [("p", Ignore)])
        , ("t2STC2_PRE",        FormOverride [("p", Ignore)])
        , ("t2STCL_OFFSET",     FormOverride [("p", Ignore)])
        , ("t2STCL_OPTION",     FormOverride [("p", Ignore)])
        , ("t2STCL_POST",       FormOverride [("p", Ignore)])
        , ("t2STCL_PRE",        FormOverride [("p", Ignore)])
        , ("t2STC_OFFSET",      FormOverride [("p", Ignore)])
        , ("t2STC_OPTION",      FormOverride [("p", Ignore)])
        , ("t2STC_POST",        FormOverride [("p", Ignore)])
        , ("t2STC_PRE",         FormOverride [("p", Ignore)])
        , ("t2STL",             FormOverride [("p", Ignore)])
        , ("t2STLB",            FormOverride [("p", Ignore)])
        , ("t2STLEX",           FormOverride [("p", Ignore)])
        , ("t2STLEXB",          FormOverride [("p", Ignore)])
        , ("t2STLEXD",          FormOverride [("p", Ignore)])
        , ("t2STLEXH",          FormOverride [("p", Ignore)])
        , ("t2STLH",            FormOverride [("p", Ignore)])
        , ("t2STMDB",           FormOverride [("p", Ignore)])
        , ("t2STMDB_UPD",       FormOverride [("wb", Ignore), ("p", Ignore)])
        , ("t2STMIA",           FormOverride [("p", Ignore)])
        , ("t2STMIA_UPD",       FormOverride [("wb", Ignore), ("p", Ignore)])
        , ("t2STRBT",           FormOverride [("p", Ignore)])
        , ("t2STRB_POST",       FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2STRB_PRE",        FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2STRBi12",         FormOverride [("p", Ignore)])
        , ("t2STRBi8",          FormOverride [("p", Ignore)])
        , ("t2STRBs",           FormOverride [("p", Ignore)])
        , ("t2STRD_POST",       FormOverride [("wb", Ignore), ("p", Ignore)])
        , ("t2STRD_PRE",        FormOverride [("wb", Ignore), ("p", Ignore)])
        , ("t2STRDi8",          FormOverride [("p", Ignore)])
        , ("t2STREX",           FormOverride [("p", Ignore)])
        , ("t2STREXB",          FormOverride [("p", Ignore)])
        , ("t2STREXD",          FormOverride [("p", Ignore)])
        , ("t2STREXH",          FormOverride [("p", Ignore)])
        , ("t2STRHT",           FormOverride [("p", Ignore)])
        , ("t2STRH_POST",       FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2STRH_PRE",        FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2STRHi12",         FormOverride [("p", Ignore)])
        , ("t2STRHi8",          FormOverride [("p", Ignore)])
        , ("t2STRHs",           FormOverride [("p", Ignore)])
        , ("t2STRT",            FormOverride [("p", Ignore)])
        , ("t2STR_POST",        FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2STR_PRE",         FormOverride [("Rn_wb", Ignore), ("p", Ignore)])
        , ("t2STRi12",          FormOverride [("p", Ignore)])
        , ("t2STRi8",           FormOverride [("p", Ignore)])
        , ("t2STRs",            FormOverride [("p", Ignore)])
        , ("t2SUBS_PC_LR",      FormOverride [("p", Ignore)])
        , ("t2SUBri",           FormOverride [("p", Ignore)])
        , ("t2SUBri12",         FormOverride [("p", Ignore)])
        , ("t2SUBrr",           FormOverride [("p", Ignore)])
        , ("t2SUBrs",           FormOverride [("p", Ignore)])
        , ("t2SXTAB",           FormOverride [("p", Ignore)])
        , ("t2SXTAB16",         FormOverride [("p", Ignore)])
        , ("t2SXTAH",           FormOverride [("p", Ignore)])
        , ("t2SXTB",            FormOverride [("p", Ignore)])
        , ("t2SXTB16",          FormOverride [("p", Ignore)])
        , ("t2SXTH",            FormOverride [("p", Ignore)])
        , ("t2TBB",             FormOverride [("addr", Ignore), ("p", Ignore)])
        , ("t2TBH",             FormOverride [("addr", Ignore), ("p", Ignore)])
        , ("t2TEQri",           FormOverride [("p", Ignore)])
        , ("t2TEQrr",           FormOverride [("p", Ignore)])
        , ("t2TEQrs",           FormOverride [("p", Ignore)])
        , ("t2TSTri",           FormOverride [("p", Ignore)])
        , ("t2TSTrr",           FormOverride [("p", Ignore)])
        , ("t2TSTrs",           FormOverride [("p", Ignore)])
        , ("t2TT",              FormOverride [("p", Ignore)])
        , ("t2TTA",             FormOverride [("p", Ignore)])
        , ("t2TTAT",            FormOverride [("p", Ignore)])
        , ("t2TTT",             FormOverride [("p", Ignore)])
        , ("t2UADD16",          FormOverride [("p", Ignore)])
        , ("t2UADD8",           FormOverride [("p", Ignore)])
        , ("t2UASX",            FormOverride [("p", Ignore)])
        , ("t2UBFX",            FormOverride [("p", Ignore)])
        , ("t2UDIV",            FormOverride [("p", Ignore)])
        , ("t2UHADD16",         FormOverride [("p", Ignore)])
        , ("t2UHADD8",          FormOverride [("p", Ignore)])
        , ("t2UHASX",           FormOverride [("p", Ignore)])
        , ("t2UHSAX",           FormOverride [("p", Ignore)])
        , ("t2UHSUB16",         FormOverride [("p", Ignore)])
        , ("t2UHSUB8",          FormOverride [("p", Ignore)])
        , ("t2UMAAL",           FormOverride [("RLo", Ignore), ("RHi", Ignore), ("p", Ignore)])
        , ("t2UMLAL",           FormOverride [("RLo", Ignore), ("RHi", Ignore), ("p", Ignore)])
        , ("t2UMULL",           FormOverride [("p", Ignore)])
        , ("t2UQADD16",         FormOverride [("p", Ignore)])
        , ("t2UQADD8",          FormOverride [("p", Ignore)])
        , ("t2UQASX",           FormOverride [("p", Ignore)])
        , ("t2UQSAX",           FormOverride [("p", Ignore)])
        , ("t2UQSUB16",         FormOverride [("p", Ignore)])
        , ("t2UQSUB8",          FormOverride [("p", Ignore)])
        , ("t2USAD8",           FormOverride [("p", Ignore)])
        , ("t2USADA8",          FormOverride [("p", Ignore)])
        , ("t2USAT",            FormOverride [("p", Ignore)])
        , ("t2USAT16",          FormOverride [("p", Ignore)])
        , ("t2USAX",            FormOverride [("p", Ignore)])
        , ("t2USUB16",          FormOverride [("p", Ignore)])
        , ("t2USUB8",           FormOverride [("p", Ignore)])
        , ("t2UXTAB",           FormOverride [("p", Ignore)])
        , ("t2UXTAB16",         FormOverride [("p", Ignore)])
        , ("t2UXTAH",           FormOverride [("p", Ignore)])
        , ("t2UXTB",            FormOverride [("p", Ignore)])
        , ("t2UXTB16",          FormOverride [("p", Ignore)])
        , ("t2UXTH",            FormOverride [("p", Ignore)])
        , ("tADC",              FormOverride [("s", Ignore), ("Rn", Ignore), ("p", Ignore)])
        , ("tBIC",              FormOverride [("s", Ignore), ("Rn", Ignore), ("p", Ignore)])
        , ("tEOR",              FormOverride [("s", Ignore), ("Rn", Ignore), ("p", Ignore)])
        , ("tLSLri",            FormOverride [("s", Ignore), ("s", Ignore), ("s", Ignore)])
        , ("tLSLrr",            FormOverride [("s", Ignore), ("s", Ignore), ("s", Ignore)])
        , ("tLSRri",            FormOverride [("s", Ignore), ("p", Ignore)])
        , ("tLSRrr",            FormOverride [("s", Ignore), ("Rn", Ignore), ("p", Ignore)])
        , ("tMOVi8",            FormOverride [("s", Ignore), ("p", Ignore)])
        , ("tMUL",              FormOverride [("s", Ignore), ("Rm", Ignore), ("p", Ignore)])
        , ("tMVN",              FormOverride [("s", Ignore), ("p", Ignore)])
        , ("tORR",              FormOverride [("s", Ignore), ("Rn", Ignore), ("p", Ignore)])
        , ("tROR",              FormOverride [("s", Ignore), ("Rn", Ignore), ("p", Ignore)])
        , ("tRSB",              FormOverride [("s", Ignore), ("p", Ignore)])
        , ("tSBC",              FormOverride [("s", Ignore), ("Rn", Ignore), ("p", Ignore)])
        , ("tSUBi3",            FormOverride [("s", Ignore), ("p", Ignore)])
        , ("tSUBi8",            FormOverride [("s", Ignore), ("Rn", Ignore), ("p", Ignore)])
        , ("tSUBrr",            FormOverride [("s", Ignore), ("p", Ignore)])
        , ("tADDi3",            FormOverride [("s", Ignore), ("p", Ignore)])
        , ("tADDi8",            FormOverride [("s", Ignore), ("Rn", Ignore), ("p", Ignore)])
        , ("tADDrr",            FormOverride [("s", Ignore), ("p", Ignore)])
        , ("tASRri",            FormOverride [("s", Ignore), ("p", Ignore)])
        , ("tASRrr",            FormOverride [("s", Ignore), ("Rn", Ignore), ("p", Ignore)])
        , ("tAND",              FormOverride [("s", Ignore), ("Rn", Ignore), ("p", Ignore)])
        , ("tADDhirr"           , FormOverride [("Rn", Ignore), ("p"  , Ignore)])
        , ("tADDrSP"            , FormOverride [("sp", Ignore), ("Rn" , Ignore), ("p", Ignore)])
        , ("tADDrSPi"           , FormOverride [("sp", Ignore), ("p"  , Ignore)])
        , ("tADDspi"            , FormOverride [("Rdn", Ignore), ("Rn" , Ignore), ("p", Ignore)])
        , ("tADDspr"            , FormOverride [("Rdn", Ignore), ("Rn" , Ignore), ("p", Ignore)])
        , ("tADR"               , FormOverride [("p", Ignore)])
        , ("tB"                 , FormOverride [("p", Ignore)])
        , ("tBL"                , FormOverride [("p", Ignore)])
        , ("tBLXNSr"            , FormOverride [("p", Ignore)])
        , ("tBLXi"              , FormOverride [("p", Ignore)])
        , ("tBLXr"              , FormOverride [("p", Ignore)])
        , ("tBX"                , FormOverride [("p", Ignore)])
        , ("tBXNS"              , FormOverride [("p", Ignore)])
        , ("tCMNz"              , FormOverride [("p", Ignore)])
        , ("tCMPhir"            , FormOverride [("p", Ignore)])
        , ("tCMPi8"             , FormOverride [("p", Ignore)])
        , ("tCMPr"              , FormOverride [("p", Ignore)])
        , ("tHINT"              , FormOverride [("p", Ignore)])
        , ("tInt_eh_sjlj_setjmp", FormOverride [("src", Ignore), ("val", Ignore)])
        , ("tLDMIA"             , FormOverride [("p", Ignore)])
        , ("tLDRBi"             , FormOverride [("p", Ignore)])
        , ("tLDRBr"             , FormOverride [("p", Ignore)])
        , ("tLDRHi"             , FormOverride [("p", Ignore)])
        , ("tLDRHr"             , FormOverride [("p", Ignore)])
        , ("tLDRSB"             , FormOverride [("p", Ignore)])
        , ("tLDRSH"             , FormOverride [("p", Ignore)])
        , ("tLDRi"              , FormOverride [("p", Ignore)])
        , ("tLDRpci"            , FormOverride [("p", Ignore)])
        , ("tLDRr"              , FormOverride [("p", Ignore)])
        , ("tLDRspi"            , FormOverride [("p", Ignore)])
        , ("tMOVr"              , FormOverride [("p", Ignore)])
        , ("tPICADD"            , FormOverride [("cp", Ignore), ("lhs", Ignore)])
        , ("tPOP"               , FormOverride [("p", Ignore)])
        , ("tPUSH"              , FormOverride [("p", Ignore)])
        , ("tREV"               , FormOverride [("p", Ignore)])
        , ("tREV16"             , FormOverride [("p", Ignore)])
        , ("tREVSH"             , FormOverride [("p", Ignore)])
        , ("tSTMIA_UPD"         , FormOverride [("wb", Ignore), ("p", Ignore)])
        , ("tSTRBi"             , FormOverride [("p", Ignore)])
        , ("tSTRBr"             , FormOverride [("p", Ignore)])
        , ("tSTRHi"             , FormOverride [("p", Ignore)])
        , ("tSTRHr"             , FormOverride [("p", Ignore)])
        , ("tSTRi"              , FormOverride [("p", Ignore)])
        , ("tSTRr"              , FormOverride [("p", Ignore)])
        , ("tSTRspi"            , FormOverride [("p", Ignore)])
        , ("tSUBspi"            , FormOverride [("Rdn", Ignore), ("Rn", Ignore), ("p", Ignore)])
        , ("tSVC"               , FormOverride [("p", Ignore)])
        , ("tSXTB"              , FormOverride [("p", Ignore)])
        , ("tSXTH"              , FormOverride [("p", Ignore)])
        , ("tTST"               , FormOverride [("p", Ignore)])
        , ("tUXTB"              , FormOverride [("p", Ignore)])
        , ("tUXTH"              , FormOverride [("p", Ignore)])
        ]

    prettyOverrides =
        [ ("tSUBspi", [("Rdn", "sp")])
        , ("tADDspi", [("Rdn", "sp")])
        ]
