{-# LANGUAGE ExistentialQuantification #-}
module Dismantle.Tablegen.TH.Bits (
  parseOperand,
  OperandWrapper(..),
  assembleBits
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Bits
import qualified Data.Foldable as F
import Data.Word ( Word8 )

import Dismantle.Tablegen.ByteTrie ( Bit(..) )

{-# INLINE parseOperand #-}
parseOperand :: (Bits b, Num b) => LBS.ByteString -> [(Int, Word8)] -> b
parseOperand bs = F.foldl' (parseBit bs) 0

{-# INLINE parseBit #-}
parseBit :: (Bits b) => LBS.ByteString -> b -> (Int, Word8) -> b
parseBit bs acc (bsIx, opBitNum)
  | testBit byte bitNum = acc `setBit` fromIntegral opBitNum
  | otherwise = acc
  where
    (byteNum, bitNum) = quotRem bsIx 8
    byte = LBS.index bs (fromIntegral byteNum)

-- | A wrapper around a field with a description of where each bit
-- goes into the result.
data OperandWrapper = forall b . (Bits b) => OperandWrapper b [(Int, Word8)]

assembleBits :: [Bit] -> [OperandWrapper] -> BS.ByteString
assembleBits = undefined
