{-# LANGUAGE ExistentialQuantification #-}
module Dismantle.Tablegen.TH.Bits (
  parseOperand,
  fieldFromWord,
  assembleBits
  ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Bits
import qualified Data.Foldable as F
import Data.Word ( Word8 )

-- | Parse a field from an input word
fieldFromWord :: (Integral a, Bits a, Num b, Bits b) => a -> Int -> Int -> b
fieldFromWord w startBit numBits =
  fromIntegral ((w .&. mask) `shiftR` startBit)
  where
    mask = ((1 `shiftL` numBits) - 1) `shiftL` startBit

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

assembleBits :: (Num b, Bits b) => b -> [(b, Int)] -> b
assembleBits requiredBitMask operands =
  F.foldl' applyOperand requiredBitMask operands

applyOperand :: (Bits b, Num b) => b -> (b, Int) -> b
applyOperand w (val, off) = w .|. (val `shiftL` off)

