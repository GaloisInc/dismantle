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
import Dismantle.Tablegen.Parser.Types ( defMetadata, defName, Metadata(..) )
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
                  not (Metadata "Pseudo" `elem` defMetadata d) &&
                  L.last (defName d) /= '8'

    ppcPseudo = idPseudo

ppcFormOverrides :: [(String, FormOverride)]
ppcFormOverrides = [ ("BForm", ppcBForm)
                   , ("BForm_1", ppcBForm)
                   , ("BForm_2", ppcBForm)
                   , ("BForm_3", ppcBForm)
                   , ("BForm_4", ppcBForm)
                   , ("DForm_base", ppcDForm)
                   , ("DForm_1", ppcDForm)
                   , ("DForm_2", ppcDForm)
                   , ("DForm_2_r0", ppcDForm_2_r0)
                   , ("DForm_3", ppcDForm)
                   , ("DForm_4", ppcDForm_4)
                   , ("DForm_5", ppcDForm_5)
                   , ("DSForm_1", ppcDSForm)
                   , ("DSS_Form", ppcDSSForm)
                   , ("DCB_Form", ppcDCBForm)
                   , ("DCB_Form_hint", ppcDCBForm)
                   , ("EVXForm_D", ppcEVXForm)
                   , ("IForm", ppcIForm)
                   , ("MForm_1", ppcMForm)
                   , ("MDForm_1", ppcMDForm)
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
                   , ("XForm_28", ppcXForm)
                   , ("XForm_42", ppcXForm_42)
                   , ("XForm_tlbws", ppcXForm)
                   , ("XForm_icbt", ppcXForm_icbt)
                   , ("XForm_base_r3xo", ppcXForm)
                   , ("XFXForm_1", ppcXFXForm)
                   , ("XFXForm_3p", ppcXFXForm)
                   , ("XFXForm_5a", ppcXFXForm_5a)
                   , ("XLForm_S", ppcXLForm)
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
                   , ("DQ_RD6_RS5_DQ12", ppcDSForm)
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
                            , ("sym", SimpleDescriptor "C")
                            ]

    ppcDForm_2_r0 = FormOverride [ ("rD", SimpleDescriptor "A")
                                 , ("imm", SimpleDescriptor "B")
                                 ]

    ppcDForm_4 = FormOverride [ ("dst", SimpleDescriptor "B")
                              , ("src1", SimpleDescriptor "A")
                              , ("src2", SimpleDescriptor "C")
                              ]

    ppcDForm_5 = FormOverride [ ("imm", SimpleDescriptor "I")
                              , ("crD", SimpleDescriptor "BF")
                              , ("rA", SimpleDescriptor "RA")
                              , ("src2", SimpleDescriptor "I")
                              , ("dst", SimpleDescriptor "BF")
                              , ("src1", SimpleDescriptor "RA")
                              ]

    ppcDSForm = FormOverride [ ("rS", SimpleDescriptor "RST")
                             , ("vD", SimpleDescriptor "RST")
                             , ("rD", SimpleDescriptor "RST")
                             , ("vS", SimpleDescriptor "RST")
                             , ("dst", SimpleDescriptor "DS_RA")
                             , ("src", SimpleDescriptor "DS_RA")
                             , ("addr", SimpleDescriptor "DS_RA")
                             ]

    ppcDCBForm = FormOverride [ ("dst", ComplexDescriptor (("B", 0) NL.:| [("A", 5)]))
                              ]

    ppcDSSForm = FormOverride [ ("rA", SimpleDescriptor "A")
                              , ("rB", SimpleDescriptor "B")
                              ]

    ppcEVXForm = FormOverride [ ("dst", SimpleDescriptor "D")
                              ]

    ppcIForm = FormOverride [ ("dst", SimpleDescriptor "LI")
                            , ("func", SimpleDescriptor "LI")
                            ]

    ppcMForm = FormOverride [ ("SH", SimpleDescriptor "RB")
                            , ("rA", SimpleDescriptor "RA")
                            , ("rB", SimpleDescriptor "RB")
                            , ("rSi", SimpleDescriptor "RS")
                            , ("rS", SimpleDescriptor "RS")
                            ]

    ppcMDForm = FormOverride [ ("rA", SimpleDescriptor "RA")
                             , ("rS", SimpleDescriptor "RS")
                             , ("rSi", SimpleDescriptor "RS")
                             ]

    ppcVXForm = FormOverride [ ("vD", SimpleDescriptor "VD")
                             , ("vB", SimpleDescriptor "VB")
                             , ("rA", SimpleDescriptor "VA")
                             , ("rD", SimpleDescriptor "VD")
                             , ("UIMM", SimpleDescriptor "VA")
                             , ("SIMM", SimpleDescriptor "IMM")
                             ]

    ppcXForm_icbt = FormOverride [ ("src", ComplexDescriptor (("RB", 0) NL.:| [("RA", 5)]))
                                 ]

    ppcXForm = FormOverride [ ("rA", SimpleDescriptor "A")
                            , ("vA", SimpleDescriptor "A")
                            , ("frA", SimpleDescriptor "A")
                            , ("rB", SimpleDescriptor "B")
                            , ("RB", SimpleDescriptor "B")
                            , ("vB", SimpleDescriptor "B")
                            , ("frB", SimpleDescriptor "B")
                            , ("SH", SimpleDescriptor "B")
                            , ("FC", SimpleDescriptor "B")
                            , ("rS", SimpleDescriptor "RST")
                            , ("RS", SimpleDescriptor "RST")
                            , ("vT", SimpleDescriptor "RST")
                            , ("vD", SimpleDescriptor "RST")
                            , ("to", SimpleDescriptor "RST")
                            , ("RTS", SimpleDescriptor "RST")
                            , ("frD", SimpleDescriptor "RST")
                            , ("frS", SimpleDescriptor "RST")
                            , ("rD", SimpleDescriptor "RST")
                            , ("RT", SimpleDescriptor "RST")
                            , ("VA", SimpleDescriptor "FRA")
                            , ("fA", SimpleDescriptor "FRA")
                            , ("VB", SimpleDescriptor "FRB")
                            , ("fB", SimpleDescriptor "FRB")
                            , ("crD", SimpleDescriptor "BF")
                            , ("dst", ComplexDescriptor (("B", 0) NL.:| [("A", 5)]))
                            , ("src", ComplexDescriptor (("B", 0) NL.:| [("A", 5)]))
                            , ("addr", ComplexDescriptor (("B", 0) NL.:| [("A", 5)]))
                            , ("ptr", ComplexDescriptor (("B", 0) NL.:| [("A", 5)]))
                            ]

    ppcXForm_8 = FormOverride [ ("rS", SimpleDescriptor "RST")
                              , ("dst", ComplexDescriptor (("B", 0) NL.:| [("A", 5)]))
                              ]

    ppcXForm_16 = FormOverride [ ("rA", SimpleDescriptor "RA")
                               , ("rB", SimpleDescriptor "RB")
                               , ("crD", SimpleDescriptor "BF")
                               ]

    ppcXForm_42 = FormOverride [ ("rT", SimpleDescriptor "RST")
                               ]

    ppcXLForm = FormOverride [ ("imm", SimpleDescriptor "S")
                             ]

    ppcXOForm = FormOverride [ ("rT", SimpleDescriptor "RT")
                             , ("rB", SimpleDescriptor "RB")
                             , ("rA", SimpleDescriptor "RA")
                             ]

    ppcXFXForm = FormOverride [ ("rS", SimpleDescriptor "RT")
                              , ("rD", SimpleDescriptor "RT")
                              , ("imm", SimpleDescriptor "Entry")
                              , ("dmy", SimpleDescriptor "Entry")
                              ]

    ppcXFXForm_5a = FormOverride [ ("rT", SimpleDescriptor "ST")
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
                             , ("src", ComplexDescriptor (("B", 0) NL.:| [("A", 5)]))
                             , ("rA", SimpleDescriptor "A")
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
  , ("U10imm", unsignedImmediate 10)
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
