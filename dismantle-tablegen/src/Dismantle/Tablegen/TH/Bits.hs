{-# LANGUAGE ExistentialQuantification #-}
module Dismantle.Tablegen.TH.Bits (
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

assembleBits :: (Num b, Bits b) => b -> [(b, Int)] -> b
assembleBits requiredBitMask operands =
  F.foldl' applyOperand requiredBitMask operands

applyOperand :: (Bits b, Num b) => b -> (b, Int) -> b
applyOperand w (val, off) = w .|. (val `shiftL` off)

