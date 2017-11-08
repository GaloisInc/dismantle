{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.PPC.ISA (
  isa
  ) where

import GHC.TypeLits

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as LBS
import Data.Int ( Int16, Int64 )
import qualified Data.List.NonEmpty as NL
import qualified Data.List as L
import Data.Proxy ( Proxy(..) )
import Data.Word ( Word8, Word32, Word64 )

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Data.Int.Indexed as I
import qualified Data.Word.Indexed as I
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
          , isaMapOperandPayloadType = mapOperandPayloadType
          , isaDefaultPrettyVariableValues = []
          , isaPrettyOverrides = []
          , isaUnusedBitsPolicy = Nothing
          }
  where
    ppcIgnoreOperand op = op `elem` [ "ptr_rc_nor0:$ea_res"
                                    , "ptr_rc_nor0:$ea_result"
                                    , "crrc0:$ret"
                                    , "crrc:$ret"
                                    ]

    -- The instructions we actually want to generate opcodes for.
    --
    -- We restrict ourselves to non-pseudo instructions declaring themselves in
    -- the PPC namespace.  Furthermore, we exclude the SPE (Streaming Processor
    -- Extensions) instructions because they are uncommon; those instructions
    -- all have an EV prefix.
    ppcFilter d = hasNamedString "Namespace" "PPC" d &&
                  hasNamedString "DecoderNamespace" "" d &&
                  not (Metadata "Pseudo" `elem` defMetadata d) &&
                  not (L.isPrefixOf "EV" (defName d)) &&
                  not (L.isInfixOf "TLS" (defName d)) &&
                  not ('8' `elem` defName d)

    ppcPseudo = idPseudo

    mapOperandPayloadType s =
      case s of
        "g8rc" -> "gprc"
        "f4rc" -> "fprc"
        "f8rc" -> "fprc"
        _ -> s

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
                   , ("XForm_42", ppcXForm)
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
                            , ("rSi", Ignore)
                            , ("rS", SimpleDescriptor "RS")
                            ]

    ppcMDForm = FormOverride [ ("rA", SimpleDescriptor "RA")
                             , ("rS", SimpleDescriptor "RS")
                             , ("rSi", Ignore)
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
                            , ("rT", SimpleDescriptor "RST")
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
  [ ("Abscondbrtarget", condAbsoluteAddress)
  , ("Absdirectbrtarget", absoluteAddress)
  , ("Condbrtarget", condRelativeOffset)
  , ("Directbrtarget", relativeOffset)
  , ("Calltarget", relativeOffset)
  , ("Abscalltarget", absoluteAddress)
  , ("Tlscall", relativeOffset)
  , ("Tlscall32", relativeOffset)
  , ("Crbitm", crbitm)  -- I think these following 3 are correct now.
  , ("Crbitrc", crbitrc)
  , ("Crrc", crrc) -- 4 bit
  , ("Fprc", floatRegister)
  , ("Gprc", gpRegister)
    -- These two variants are special for instructions that treat r0 specially
  , ("Gprc_nor0", gpRegister)
  , ("I1imm", signedImmediate (Proxy :: Proxy 1))
  , ("I32imm", signedImmediate (Proxy :: Proxy 32)) -- fixme
  , ("S16imm", s16Imm)
  , ("S16imm64", s16Imm)
  -- The s17imm types are actually stored as 16 bits in the instruction.  They
  -- are also rendered in 16 bit form in assembly.  Their *interpretation* is as
  -- if they were shifted left by 4 bits and then sign extended.
  --
  -- We keep them in their simplified form here; the semantics will have to
  -- account for the shifting.
  , ("S17imm", s16Imm)
  , ("S17imm64", s16Imm)
  , ("S5imm", signedImmediate (Proxy :: Proxy 5))
  , ("Tlsreg", gpRegister)
  , ("Tlsreg32", gpRegister)
  , ("U1imm", unsignedImmediate (Proxy :: Proxy 1))
  , ("U2imm", unsignedImmediate (Proxy :: Proxy 2))
  , ("U4imm", unsignedImmediate (Proxy :: Proxy 4))
  , ("U5imm", unsignedImmediate (Proxy :: Proxy 5))
  , ("U6imm", unsignedImmediate (Proxy :: Proxy 6))
  , ("U7imm", unsignedImmediate (Proxy :: Proxy 7))
  , ("U8imm", unsignedImmediate (Proxy :: Proxy 8))
  , ("U10imm", unsignedImmediate (Proxy :: Proxy 10))
  , ("U16imm", unsignedImmediate (Proxy :: Proxy 16))
  , ("U16imm64", unsignedImmediate (Proxy :: Proxy 16))
  , ("Memrr", memRR)
  , ("Memri", memRI)
  , ("Memrix", memRIX)
  , ("Memrix16", memRIX)
  , ("Pred", gpRegister)
  , ("Vrrc", vecRegister)
  , ("Vsfrc", vsRegister)
  , ("Vsrc", vsRegister)
  , ("Vssrc", vsRegister)
  ]
  where
    absoluteAddress = OperandPayload { opTypeT = [t| PPC.AbsBranchTarget |]
                                     , opConE = Just (varE 'PPC.mkAbsBranchTarget)
                                     , opWordE = Just (varE 'PPC.absBranchTargetToBits)
                                     }
    relativeOffset = OperandPayload { opTypeT = [t| PPC.BranchTarget |]
                                    , opConE = Just (varE 'PPC.mkBranchTarget)
                                    , opWordE = Just (varE 'PPC.branchTargetToBits)
                                    }
    condAbsoluteAddress = OperandPayload { opTypeT = [t| PPC.AbsCondBranchTarget |]
                                         , opConE = Just (varE 'PPC.mkAbsCondBranchTarget)
                                         , opWordE = Just (varE 'PPC.absCondBranchTargetToBits)
                                         }
    condRelativeOffset = OperandPayload { opTypeT = [t| PPC.CondBranchTarget |]
                                        , opConE = Just (varE 'PPC.mkCondBranchTarget)
                                        , opWordE = Just (varE 'PPC.condBranchTargetToBits)
                                        }
    gpRegister = OperandPayload { opTypeT = [t| PPC.GPR |]
                                , opConE = Just (conE 'PPC.GPR)
                                , opWordE = Just [| fromIntegral . PPC.unGPR |]
                                }
    crbitm = OperandPayload { opTypeT = [t| PPC.CRBitM |]
                            , opConE = Just (varE 'PPC.mkCRBitM)
                            , opWordE = Just (varE 'PPC.crbitmToBits)
                            }
    crbitrc = OperandPayload { opTypeT = [t| PPC.CRBitRC |]
                             , opConE = Just [| PPC.CRBitRC |]
                             , opWordE = Just [| fromIntegral . PPC.unCRBitRC |]
                             }
    crrc = OperandPayload { opTypeT = [t| PPC.CRRC |]
                          , opConE = Just [| PPC.CRRC |]
                          , opWordE = Just [| fromIntegral . PPC.unCRRC |]
                          }
    floatRegister = OperandPayload { opTypeT = [t| PPC.FR |]
                                   , opConE = Just (conE 'PPC.FR)
                                   , opWordE = Just [| fromIntegral . PPC.unFR |]
                                   }

    s16Imm = OperandPayload { opTypeT = [t| Int16 |]
                            , opConE = Nothing
                            , opWordE = Just [| truncBits 16 |]
                            }

    signedImmediate :: forall (n :: Nat) . (KnownNat n) => Proxy n -> OperandPayload
    signedImmediate p = OperandPayload { opTypeT = [t| I.I $(return (LitT (NumTyLit (natVal p)))) |]
                                       , opConE = Just [| I.I |]
                                       , opWordE = Just [| PPC.signedImmediateToWord32 |]
                                       }
    unsignedImmediate :: forall (n :: Nat) . (KnownNat n) => Proxy n -> OperandPayload
    unsignedImmediate p = OperandPayload { opTypeT = [t| I.W $(return (LitT (NumTyLit (natVal p)))) |]
                                         , opConE = Just [| I.w |]
                                         , opWordE = Just [| fromIntegral . I.unW |]
                                         }
    vecRegister = OperandPayload { opTypeT = [t| PPC.VR |]
                                 , opConE = Just (conE 'PPC.VR)
                                 , opWordE = Just [| fromIntegral . PPC.unVR |]
                                 }
    vsRegister = OperandPayload { opTypeT = [t| PPC.VSReg |]
                                , opConE = Just (conE 'PPC.VSReg)
                                , opWordE = Just [| fromIntegral . PPC.unVSReg |]
                                }
    memRI = OperandPayload { opTypeT = [t| PPC.MemRI |]
                         , opConE = Just (varE 'PPC.mkMemRI)
                         , opWordE = Just (varE 'PPC.memRIToBits)
                         }
    memRIX = OperandPayload { opTypeT = [t| PPC.MemRIX |]
                         , opConE = Just (varE 'PPC.mkMemRIX)
                         , opWordE = Just (varE 'PPC.memRIXToBits)
                         }
    memRR = OperandPayload { opTypeT = [t| PPC.MemRR |]
                           , opConE = Just (varE 'PPC.mkMemRR)
                           , opWordE = Just (varE 'PPC.memRRToBits)
                           }
