{-# LANGUAGE ViewPatterns #-}
-- | This module defines some helpers called in the TemplateHaskell code
--
-- The functions here mostly focus on extracting operands and re-assembling
-- operands back into instructions.
module Dismantle.Tablegen.TH.Bits (
  fieldFromWord,
  assembleBits
  ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Bits
import qualified Data.Foldable as F
import Data.Word ( Word8 )

-- | Parse a field (operand) from an input word
--
-- Note that operands are *not* necessarily contiguous.  We have to have all of
-- the bit numbers, divide them into chunks (usually one chunk, but not always).
fieldFromWord :: (Integral w, Bits w, Num r, Bits r) => w -> [(Int, Word8, Word8)] -> r
fieldFromWord w = foldr parseAndMergeChunk 0
  where
    parseAndMergeChunk (instructionBit, fromIntegral -> operandBit, fromIntegral -> chunkSize) acc =
      let mask = ((1 `shiftL` chunkSize) - 1) `shiftL` instructionBit
          opBits = (w .&. mask) `shiftR` instructionBit
      in acc .|. fromIntegral (opBits `shiftL` operandBit)

-- fieldFromWord :: (Integral a, Bits a, Num b, Bits b) => a -> Int -> Int -> b
-- fieldFromWord w startBit numBits =
--   fromIntegral ((w .&. mask) `shiftR` startBit)
--   where
--     mask = ((1 `shiftL` numBits) - 1) `shiftL` startBit

assembleBits :: (Num b, Bits b) => b -> [(b, Int)] -> b
assembleBits requiredBitMask operands =
  F.foldl' applyOperand requiredBitMask operands

applyOperand :: (Bits b, Num b) => b -> (b, Int) -> b
applyOperand w (val, off) = w .|. (val `shiftL` off)

