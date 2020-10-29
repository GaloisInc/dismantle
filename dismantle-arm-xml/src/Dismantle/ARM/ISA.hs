{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module           : Dismantle.ARM.ISA
Copyright        : (c) Galois, Inc 2019-2020
Maintainer       : Daniel Matichuk <dmatichuk@galois.com>

Specification of the A32 and T32 ISAs.

-}
module Dismantle.ARM.ISA (
  isa
  ) where

import           GHC.TypeLits ( KnownNat )

import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import           Data.Bits ( (.&.), (.|.), shiftL, shiftR )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.Split as L
import           Data.Word ( Word16, Word32 )
import qualified Data.Word.Indexed as W
import qualified Language.Haskell.TH as TH
import qualified Text.PrettyPrint.HughesPJClass as PP
import           Text.Printf (printf)

import qualified Data.Word.Indexed as I
import qualified Dismantle.Tablegen as DA

fullWordStartPatterns :: [Word16]
fullWordStartPatterns =
    [ 0b11101 `shiftL` 11
    , 0b11110 `shiftL` 11
    , 0b11111 `shiftL` 11
    ]

asWord32 :: LBS.ByteString -> Word32
asWord32 = BG.runGet $ do
    w1 <- BG.getWord16le
    -- These bit patterns indicate a full word instruction so we need to
    -- parse another halfword and shift the first word left.
    --
    -- For details, see the ARM ARM A6.1 Thumb Instruction Set Encoding,
    -- version ARM DDI 0406C.b.
    let matchesPattern p = (w1 .&. p) == p
    let fullWord = any matchesPattern fullWordStartPatterns
    if not fullWord
       then return $ fromIntegral w1
       else do
           w2 <- BG.getWord16le
           return $ (((fromIntegral w1)::Word32) `shiftL` 16) .|.
                    (fromIntegral w2)

fromWord32 :: Word32 -> LBS.ByteString
fromWord32 w
  | fullWord = BP.runPut $ do
      -- Put the high 16 bits, then just the low 16 bits
      BP.putWord16le (fromIntegral $ w `shiftR` 16)
      BP.putWord16le (fromIntegral $ w .&. halfWordMask)
  | otherwise = BP.runPut $ BP.putWord16le (fromIntegral $ w .&. halfWordMask)
  where
    matchesPattern p =
            let p' = ((fromIntegral p) :: Word32) `shiftL` 16
            in (w .&. p') == p'
    fullWord :: Bool
    fullWord = any matchesPattern fullWordStartPatterns
    halfWordMask :: Word32
    halfWordMask = 0x0000ffff

-- | Create a 'DA.ISA' with the given name (either "A32" or "T32").
isa :: String -> DA.ISA
isa archName = DA.ISA { DA.isaName = archName
                      , DA.isaTgenBitPreprocess = id
                      , DA.isaInputEndianness = DA.Little LBS.reverse (concat . reverse . L.chunksOf 8)
                      , DA.isaUnusedBitsPolicy = Nothing
                      , DA.isaInstructionFilter = error "isaInstructionFilter is not used for XML"
                      , DA.isaPseudoInstruction = const False
                      , DA.isaOperandPayloadTypes = operandPayloadTypes
                      , DA.isaIgnoreOperand = const False
                      , DA.isaFormOverrides = []
                      , DA.isaPrettyOverrides = []
                      , DA.isaInsnWordFromBytes = 'asWord32
                      , DA.isaInsnWordToBytes = 'fromWord32
                      , DA.isaInsnAssembleType = ''Word32
                      , DA.isaMapOperandPayloadType = id
                      , DA.isaDefaultPrettyVariableValues = []
                      }

-- | In the XML specs, all fields are unsigned bitvectors (which are sometimes
-- treated as signed)
operandPayloadTypes :: [(String, DA.OperandPayload)]
operandPayloadTypes =
  (map (\sz -> (printf "Bv%d" sz, unsignedImmediate sz)) [1..24])
  ++
  (map (\sz -> (printf "QuasiMask%d" sz, quasimaskop sz)) [1..16])
  where
    unsignedImmediate :: Integer -> DA.OperandPayload
    unsignedImmediate sz = DA.OperandPayload { DA.opTypeT = [t| I.W $(return (TH.LitT (TH.NumTyLit sz))) |]
                                            , DA.opConE = Just [| I.w |]
                                            , DA.opWordE = Just [| fromIntegral . I.unW |]
                                            }

    quasimaskop :: Integer -> DA.OperandPayload
    quasimaskop sz = DA.OperandPayload { DA.opTypeT = [t| QuasiMask $(return (TH.LitT (TH.NumTyLit sz))) |]
                                       , DA.opConE = Just (TH.varE 'quasimask)
                                       , DA.opWordE = Just [| fromIntegral . W.unW . unQuasiMask |]
                                       }
-- | QuasiMask / Psuedo-operand

newtype QuasiMask n = QuasiMask { unQuasiMask :: W.W n }
  deriving (Eq, Ord, Show)

quasimask :: KnownNat n => Word32 -> QuasiMask n
quasimask = QuasiMask . fromIntegral


instance KnownNat n => PP.Pretty (QuasiMask n) where
  pPrint (QuasiMask w) = PP.text "QuasiMask" <> PP.pPrint "(" <> PP.pPrint (W.width w) <> PP.text "): " <> PP.pPrint w
