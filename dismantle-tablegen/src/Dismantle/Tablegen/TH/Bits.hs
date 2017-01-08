module Dismantle.Tablegen.TH.Bits (
  parseOperand
  ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Bits
import Data.Word ( Word8 )

import Dismantle.Tablegen.Types

{-# INLINE parseOperand #-}
parseOperand :: (Bits b, Num b) => OperandDescriptor -> LBS.ByteString -> b
parseOperand od bs = foldr (parseBit bs) 0 (opBits od)

{-# INLINE parseBit #-}
parseBit :: (Bits b) => LBS.ByteString -> (Int, Word8) -> b -> b
parseBit bs (bsIx, opBitNum) acc
  | testBit byte bitNum = acc `setBit` fromIntegral opBitNum
  | otherwise = acc
  where
    (byteNum, bitNum) = quotRem bsIx 8
    byte = LBS.index bs (fromIntegral byteNum)
