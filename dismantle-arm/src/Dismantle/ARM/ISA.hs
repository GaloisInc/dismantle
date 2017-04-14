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
          }
  where
    absoluteAddress = OperandPayload { opTypeName = ''Word64
                                     , opConName = Nothing
                                     , opConE = Nothing
                                     , opWordE = Just [| fromIntegral |]
                                     }
    relativeOffset = OperandPayload { opTypeName = ''Int64
                                    , opConName = Nothing
                                    , opConE = Nothing
                                    , opWordE = Just [| fromIntegral |]
                                    }
    gpRegister = OperandPayload { opTypeName = ''ARM.GPR
                                , opConName = Just 'ARM.GPR
                                , opConE = Just (conE 'ARM.GPR)
                                , opWordE = Just [| fromIntegral . ARM.unGPR |]
                                }
    conditionRegister = OperandPayload { opTypeName = ''ARM.CR
                                       , opConName = Just 'ARM.CR
                                       , opConE = Just (conE 'ARM.CR)
                                       , opWordE = Just [| fromIntegral . ARM.unCR |]
                                       }
    floatRegister = OperandPayload { opTypeName = ''ARM.FR
                                   , opConName = Just 'ARM.FR
                                   , opConE = Just (conE 'ARM.FR)
                                   , opWordE = Just [| fromIntegral . ARM.unFR |]
                                   }
    signedImmediate :: Word8 -> OperandPayload
    signedImmediate _n = OperandPayload { opTypeName = ''Int64
                                        , opConName = Nothing
                                        , opConE = Nothing
                                        , opWordE = Just [| fromIntegral |]
                                        }
    unsignedImmediate :: Word8 -> OperandPayload
    unsignedImmediate _n = OperandPayload { opTypeName = ''Word64
                                          , opConName = Nothing
                                          , opConE = Nothing
                                          , opWordE = Just [| fromIntegral |]
                                          }
    vecRegister = OperandPayload { opTypeName = ''ARM.VR
                                 , opConName = Just 'ARM.VR
                                 , opConE = Just (conE 'ARM.VR)
                                 , opWordE = Just [| fromIntegral . ARM.unVR |]
                                 }
    mem = OperandPayload { opTypeName = ''ARM.Mem
                         , opConName = Just 'ARM.mkMem
                         , opConE = Just (varE 'ARM.mkMem)
                         , opWordE = Just (varE 'ARM.memToBits)
                         }
    armOperandPayloadTypes =
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

    armFilter i = and [ idNamespace i == "ARM"
                      , idDecoder i == ""
                      , L.last (idMnemonic i) /= '8'
                      ]
    armPseudo i = idPseudo i ||
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
