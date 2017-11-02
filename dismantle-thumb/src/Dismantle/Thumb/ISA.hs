{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.Thumb.ISA (
  isa
  ) where

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as S
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

fullWordStartPatterns :: [Word16]
fullWordStartPatterns =
    [ 0b11101 `shiftL` 11
    , 0b11110 `shiftL` 11
    , 0b11111 `shiftL` 11
    ]

asWord32 :: LBS.ByteString -> Word32
asWord32 = B.runGet $ do
    w1 <- B.getWord16be
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
           w2 <- B.getWord16be
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
       then B.runPut $ B.putWord32be w
       else B.runPut $ B.putWord16be (fromIntegral $ w .&. halfWordMask)

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
          , isaUnusedBitsPolicy = Just Drop
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
    addrModeRr = OperandPayload { opTypeT = [t| Thumb.AddrModeRr |]
                                , opConE = Just (varE 'Thumb.mkAddrModeRr)
                                , opWordE = Just (varE 'Thumb.addrModeRrToBits)
                                }
    predOperand = OperandPayload { opTypeT = [t| Thumb.Pred |]
                                 , opConE = Just (varE 'Thumb.mkPred)
                                 , opWordE = Just (varE 'Thumb.predToBits)
                                 }
    tAdrLabelOperand = OperandPayload { opTypeT = [t| Thumb.TAdrLabel |]
                                      , opConE = Just (varE 'Thumb.mkTAdrLabel)
                                      , opWordE = Just (varE 'Thumb.tAdrLabelToBits)
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
    word8Operand = OperandPayload { opTypeT = [t| Word8 |]
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
    bit = OperandPayload { opTypeT = [t| Thumb.Bit |]
                         , opConE = Just (varE 'Thumb.mkBit)
                         , opWordE = Just (varE 'Thumb.bitToBits)
                         }

    thumbOperandPayloadTypes =
        [ ("GPR"                  , gpRegister)
        , ("TGPR"                 , lowGpRegister)
        , ("T_addrmode_is1"       , addrModeIs1)
        , ("T_addrmode_is2"       , addrModeIs2)
        , ("T_addrmode_is4"       , addrModeIs4)
        , ("T_addrmode_pc"        , addrModePc)
        , ("T_addrmode_rr"        , addrModeRr)
        , ("T_addrmode_sp"        , addrModePc)
        , ("T_adrlabel"           , tAdrLabelOperand)
        , ("T_brtarget"           , tBrTargetOperand)
        , ("T_imm0_1020s4"        , addrModePc)
        , ("T_imm0_508s4"         , addrModePc)
        , ("Thumb_bl_target"      , thumbBlTarget)
        , ("Thumb_blx_target"     , thumbBlxTarget)
        , ("Thumb_bcc_target"     , word8Operand)
        , ("Thumb_cb_target"      , word8Operand)
        , ("GPRnopc"              , gpRegister)
        , ("Iflags_op"            , word8Operand)
        , ("Imm0_1"               , bit)
        , ("Imm0_15"              , opcodeOperand)
        , ("Imm0_63"              , word8Operand)
        , ("Imm0_255"             , word8Operand)
        , ("Imod_op"              , word8Operand)
        , ("Pred"                 , predOperand)
        , ("Reglist"              , reglistOperand)
        , ("Setend_op"            , bit)
        , ("Unpredictable"        , word32Operand)
        ]

    thumbFilter = hasNamedString "Namespace" "ARM" &&&
                  hasNamedString "DecoderNamespace" "Thumb" &&&
                  (not . isPseudo) &&&
                  (not . ignoredDef) &&&
                  (not . ignoredMetadata)

    thumbPseudo = idPseudo

    ignoredDef d = defName d `elem`
        [ "tADDrSP" -- See tADDhirr
        , "tADDspr" -- See tADDhirr
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
        [ ("tUXTH"              , FormOverride [("p", Ignore)])
        , ("tUXTB"              , FormOverride [("p", Ignore)])
        , ("tSXTH"              , FormOverride [("p", Ignore)])
        , ("tSXTB"              , FormOverride [("p", Ignore)])
        , ("tTST"               , FormOverride [("p", Ignore)])
        , ("tSTRr"              , FormOverride [("p", Ignore)])
        , ("tSTRi"              , FormOverride [("p", Ignore)])
        , ("tSTRspi"            , FormOverride [("p", Ignore)])
        , ("tSTRBi"             , FormOverride [("p", Ignore)])
        , ("tSTRBr"             , FormOverride [("p", Ignore)])
        , ("tSTRHi"             , FormOverride [("p", Ignore)])
        , ("tSTRHr"             , FormOverride [("p", Ignore)])
        , ("tSVC"               , FormOverride [("p", Ignore)])
        , ("tSUBspi"            , FormOverride [("Rdn", Ignore), ("Rn" , Ignore), ("p", Ignore)])
        , ("tSTMIA_UPD"         , FormOverride [("wb", Ignore), ("p"  , Ignore)])
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
        , ("tPOP"               , FormOverride [("p", Ignore)])
        , ("tPUSH"              , FormOverride [("p", Ignore)])
        , ("tREV"               , FormOverride [("p", Ignore)])
        , ("tREV16"             , FormOverride [("p", Ignore)])
        , ("tREVSH"             , FormOverride [("p", Ignore)])
        , ("tPICADD"            , FormOverride [("cp", Ignore), ("lhs", Ignore)])
        ]
