{-# LANGUAGE TemplateHaskell #-}
module Dismantle.PPC.ISA (
  isa
  ) where

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as LBS
import Data.Int ( Int64 )
import qualified Data.List.NonEmpty as NL
import qualified Data.List as L
import Data.Word ( Word8, Word32, Word64 )

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Dismantle.Tablegen.ISA
import Dismantle.Tablegen.Types
import Dismantle.Tablegen.Parser.Types (defName)
import qualified Dismantle.PPC.Operands as PPC

asWord32 :: LBS.ByteString -> Word32
asWord32 = B.runGet B.getWord32be

fromWord32 :: Word32 -> LBS.ByteString
fromWord32 = B.runPut . B.putWord32be

isa :: ISA
isa = ISA { isaName = "PPC"
          , isaInputEndianness = Big
          , isaTgenBitPreprocess = L.reverse
          , isaInstructionFilter = ppcFilter
          , isaPseudoInstruction = ppcPseudo
          , isaOperandPayloadTypes = ppcOperandPayloadTypes
          , isaIgnoreOperand = ppcIgnoreOperand
          , isaFormOverrides = ppcFormOverrides
          , isaInsnWordFromBytes = 'asWord32
          , isaInsnWordToBytes = 'fromWord32
          , isaInsnAssembleType = ''Word32
          }
  where
    ppcIgnoreOperand op = op `elem` [ "ptr_rc_nor0:$ea_res"
                                    , "ptr_rc_nor0:$ea_result"
                                    , "crrc0:$ret"
                                    , "crrc:$ret"
                                    ]


    ppcFilter d = hasNamedString "Namespace" "PPC" d &&
                  hasNamedString "DecoderNamespace" "" d &&
                  L.last (defName d) /= '8'

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

ppcFormOverrides :: [(String, FormOverride)]
ppcFormOverrides = [ ("BForm", ppcBForm)
                   , ("BForm_1", ppcBForm)
                   , ("BForm_2", ppcBForm)
                   , ("BForm_3", ppcBForm)
                   , ("DForm_base", ppcDForm)
                   , ("DForm_1", ppcDForm)
                   , ("DForm_2", ppcDForm)
                   , ("DForm_3", ppcDForm)
                   , ("DForm_4", ppcDForm_4)
                   , ("DForm_5", ppcDForm_5)
                   , ("IForm", ppcIForm)
                   , ("VXForm_1", ppcVXForm)
                   , ("VXForm_2", ppcVXForm)
                   , ("VXForm_3", ppcVXForm)
                   , ("VXForm_4", ppcVXForm)
                   , ("XForm_1", ppcXForm)
                   , ("XForm_2", ppcXForm)
                   , ("XForm_3", ppcXForm)
                   , ("XForm_4", ppcXForm)
                   , ("XForm_5", ppcXForm)
                   , ("XForm_6", ppcXForm)
                   , ("XForm_7", ppcXForm)
                   , ("XForm_8", ppcXForm_8)
                   , ("XForm_9", ppcXForm)
                   , ("XForm_10", ppcXForm)
                   , ("XForm_11", ppcXForm)
                   , ("XForm_12", ppcXForm)
                   , ("XForm_13", ppcXForm)
                   , ("XForm_14", ppcXForm)
                   , ("XForm_15", ppcXForm)
                   , ("XForm_16", ppcXForm_16)
                   , ("XForm_16b", ppcXForm)
                   , ("XForm_17", ppcXForm)
                   , ("XForm_18", ppcXForm)
                   , ("XForm_19", ppcXForm)
                   , ("XForm_26", ppcXForm)
                   , ("XForm_tlbws", ppcXForm)
                   , ("XForm_base_r3xo", ppcXForm)
                   , ("XOForm_1", ppcXOForm)
                   , ("XSForm_1", ppcXSForm)
                   , ("XX1Form", ppcXXForm)
                   , ("XX2Form_1", ppcXXForm)
                   , ("XX2Form_2", ppcXXForm)
                   , ("XX3Form", ppcXXForm)
                   , ("XX3Form_1", ppcXXForm)
                   , ("XX3Form_2", ppcXXForm)
                   , ("XX2_RD6_UIM5_RS6", ppcRD6Form)
                   , ("X_RD5_XO5_RS5", ppcRD5Form)
                   , ("XX2_RD5_XO5_RS6", ppcRD5Form)
                   , ("VXForm_RD5_XO5_RS5", ppcVXForm_RD5)
                   ]
  where
    ppcBForm = FormOverride [ ("dst", SimpleDescriptor "BD")
                            , ("bi", SimpleDescriptor "BI")
                            , ("bo", SimpleDescriptor "BO")
                            ]

    ppcDForm = FormOverride [ ("dst", SimpleDescriptor "Addr")
                            , ("src", SimpleDescriptor "Addr")
                            , ("rS", SimpleDescriptor "A")
                            , ("imm", SimpleDescriptor "C")
                            , ("to", SimpleDescriptor "A")
                            , ("rA", SimpleDescriptor "B")
                            , ("rD", SimpleDescriptor "A")
                            ]

    ppcDForm_4 = FormOverride [ ("dst", SimpleDescriptor "B")
                              , ("src1", SimpleDescriptor "A")
                              , ("src2", SimpleDescriptor "C")
                              ]

    ppcDForm_5 = FormOverride [ ("imm", SimpleDescriptor "I")
                              , ("crD", SimpleDescriptor "BF")
                              , ("rA", SimpleDescriptor "RA")
                              ]

    ppcIForm = FormOverride [ ("dst", SimpleDescriptor "LI")
                            ]

    ppcVXForm = FormOverride [ ("vD", SimpleDescriptor "VD")
                             , ("vB", SimpleDescriptor "VB")
                             , ("rA", SimpleDescriptor "VA")
                             , ("rD", SimpleDescriptor "VD")
                             , ("UIMM", SimpleDescriptor "VA")
                             , ("SIMM", SimpleDescriptor "IMM")
                             ]

    ppcXForm = FormOverride [ ("rA", SimpleDescriptor "A")
                            , ("rB", SimpleDescriptor "B")
                            , ("rS", SimpleDescriptor "RST")
                            , ("RB", SimpleDescriptor "B")
                            , ("RS", SimpleDescriptor "RST")
                            , ("vT", SimpleDescriptor "RST")
                            , ("vB", SimpleDescriptor "B")
                            , ("vA", SimpleDescriptor "A")
                            , ("VB", SimpleDescriptor "FRB")
                            , ("VA", SimpleDescriptor "FRA")
                            , ("crD", SimpleDescriptor "BF")
                            , ("to", SimpleDescriptor "RST")
                            , ("RTS", SimpleDescriptor "RST")
                            , ("frD", SimpleDescriptor "RST")
                            , ("frB", SimpleDescriptor "B")
                            ]

    ppcXForm_8 = FormOverride [ ("rS", SimpleDescriptor "RST")
                              , ("dst", ComplexDescriptor (("B", 0) NL.:| [("A", 5)]))
                              ]

    ppcXForm_16 = FormOverride [ ("rA", SimpleDescriptor "RA")
                               , ("rB", SimpleDescriptor "RB")
                               , ("crD", SimpleDescriptor "BF")
                               ]

    ppcXOForm = FormOverride [ ("rT", SimpleDescriptor "RT")
                             , ("rB", SimpleDescriptor "RB")
                             , ("rA", SimpleDescriptor "RA")
                             ]

    ppcXSForm = FormOverride [ ("rA", SimpleDescriptor "A")
                             , ("rS", SimpleDescriptor "RS")
                             ]

    ppcXXForm = FormOverride [ ("UIM", SimpleDescriptor "D")
                             , ("SHW", SimpleDescriptor "D")
                             , ("DM", SimpleDescriptor "D")
                             , ("crD", SimpleDescriptor "CR")
                             , ("XTi", SimpleDescriptor "XT")
                             , ("dst", SimpleDescriptor "B")
                             ]

    ppcVXForm_RD5 = FormOverride [ ("vB", SimpleDescriptor "VB")
                                 , ("vD", SimpleDescriptor "RD")
                                 ]
    ppcRD5Form = FormOverride [ ("rT", SimpleDescriptor "RT")
                              , ("vB", SimpleDescriptor "B")
                              , ("vT", SimpleDescriptor "RST")
                              ]
    ppcRD6Form = FormOverride [ ("UIM", SimpleDescriptor "UIM5")
                              , ("UIMM", SimpleDescriptor "UIM5")
                              , ("XTi", SimpleDescriptor "XT")
                              ]

    ppcZForm = FormOverride [ ("vT", SimpleDescriptor "FRT")
                            , ("vB", SimpleDescriptor "FRB")
                            , ("rmc", SimpleDescriptor "idx")
                            ]

ppcOperandPayloadTypes =
  [ ("Abscondbrtarget", absoluteAddress)
  , ("Absdirectbrtarget", absoluteAddress)
  , ("Condbrtarget", relativeOffset)
  , ("Directbrtarget", absoluteAddress)
  , ("Calltarget", relativeOffset)
  , ("Abscalltarget", absoluteAddress)
  , ("Ptr_rc_nor0", gpRegister) -- fixme
  , ("Tlscall", gpRegister) -- fixme
  , ("Tlscall32", gpRegister) --fixme
  , ("Spe8dis", gpRegister) -- fixme
  , ("Spe2dis", gpRegister)
  , ("Spe4dis", gpRegister)
  , ("Crbitm", conditionRegister)  -- these two are very odd, must investigate
  , ("Crbitrc", conditionRegister)
  , ("Crrc", conditionRegister) -- 4 bit
  , ("F4rc", floatRegister)
  , ("F8rc", floatRegister)
  , ("G8rc", gpRegister)
  , ("Gprc", gpRegister)
    -- These two variants are special for instructions that treat r0 specially
  , ("Gprc_nor0", gpRegister)
  , ("G8rc_nox0", gpRegister)
  , ("I1imm", signedImmediate 1)
  , ("I32imm", signedImmediate 32)
  , ("S16imm", signedImmediate 16)
  , ("S16imm64", signedImmediate 16)
  , ("S17imm", signedImmediate 17)
  , ("S17imm64", signedImmediate 17)
  , ("S5imm", signedImmediate 5)
  , ("Tlsreg", gpRegister)
  , ("Tlsreg32", gpRegister)
  , ("U1imm", unsignedImmediate 1)
  , ("U2imm", unsignedImmediate 2)
  , ("U4imm", unsignedImmediate 4)
  , ("U5imm", unsignedImmediate 5)
  , ("U6imm", unsignedImmediate 6)
  , ("U7imm", unsignedImmediate 7)
  , ("U8imm", unsignedImmediate 8)
  , ("U16imm", unsignedImmediate 16)
  , ("U16imm64", unsignedImmediate 16)
  , ("Memrr", mem)
  , ("Memri", mem)
  , ("Memrix", mem)
  , ("Memrix16", mem)
  , ("Pred", gpRegister)
  , ("Vrrc", vecRegister)
  , ("Vsfrc", vecRegister) -- floating point vec?
  , ("Vsrc", vecRegister) -- ??
  , ("Vssrc", vecRegister) -- ??
  ]
  where
    absoluteAddress = OperandPayload { opTypeName = ''Word64
                                     , opConE = Nothing
                                     , opWordE = Just [| fromIntegral |]
                                     }
    relativeOffset = OperandPayload { opTypeName = ''Int64
                                    , opConE = Nothing
                                    , opWordE = Just [| fromIntegral |]
                                    }
    gpRegister = OperandPayload { opTypeName = ''PPC.GPR
                                , opConE = Just (conE 'PPC.GPR)
                                , opWordE = Just [| fromIntegral . PPC.unGPR |]
                                }
    conditionRegister = OperandPayload { opTypeName = ''PPC.CR
                                       , opConE = Just (conE 'PPC.CR)
                                       , opWordE = Just [| fromIntegral . PPC.unCR |]
                                       }
    floatRegister = OperandPayload { opTypeName = ''PPC.FR
                                   , opConE = Just (conE 'PPC.FR)
                                   , opWordE = Just [| fromIntegral . PPC.unFR |]
                                   }
    signedImmediate :: Word8 -> OperandPayload
    signedImmediate _n = OperandPayload { opTypeName = ''Int64
                                        , opConE = Nothing
                                        , opWordE = Just [| fromIntegral |]
                                        }
    unsignedImmediate :: Word8 -> OperandPayload
    unsignedImmediate _n = OperandPayload { opTypeName = ''Word64
                                          , opConE = Nothing
                                          , opWordE = Just [| fromIntegral |]
                                          }
    vecRegister = OperandPayload { opTypeName = ''PPC.VR
                                 , opConE = Just (conE 'PPC.VR)
                                 , opWordE = Just [| fromIntegral . PPC.unVR |]
                                 }
    mem = OperandPayload { opTypeName = ''PPC.Mem
                         , opConE = Just (varE 'PPC.mkMem)
                         , opWordE = Just (varE 'PPC.memToBits)
                         }
