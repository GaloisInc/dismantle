{-# LANGUAGE ViewPatterns #-}
-- | This module defines some helpers called in the TemplateHaskell code
--
-- The functions here mostly focus on extracting operands and re-assembling
-- operands back into instructions.
module Dismantle.Tablegen.TH.Bits (
  fieldFromWord,
  assembleBits
  ) where

import Data.Bits
import qualified Data.Foldable as F
import Data.Word ( Word8 )

import Dismantle.Tablegen.Types (IBit(..))
import Dismantle.Tablegen.Parser.Types (OBit(..))

-- | Parse a field (operand) from an input word
--
-- Note that operands are *not* necessarily contiguous.  We have to have all of
-- the bit numbers, divide them into chunks (usually one chunk, but not always).
fieldFromWord :: (Integral w, Bits w, Num r, Bits r) => w -> [(IBit, OBit, Word8)] -> r
fieldFromWord w = foldr parseAndMergeChunk 0
  where
    parseAndMergeChunk (IBit (fromIntegral -> instructionBit), OBit (fromIntegral -> operandBit), fromIntegral -> chunkSize) acc =
      let mask = ((1 `shiftL` chunkSize) - 1) `shiftL` instructionBit
          opBits = (w .&. mask) `shiftR` instructionBit
      in acc .|. fromIntegral (opBits `shiftL` operandBit)

-- | Project a list of operands into a word.
--
-- The intent is that the initial word contains the require bitmask of the
-- instruction, and this function places the operands into their required
-- locations.
assembleBits :: (Num b, Bits b) => b -> [(b, [(IBit, OBit, Word8)])] -> b
assembleBits requiredBitMask operands =
  F.foldl' applyOperand requiredBitMask operands

-- | Project an operand (comprised of some number of chunks) into a container
-- word @w@
applyOperand :: (Bits b, Num b) => b -> (b, [(IBit, OBit, Word8)]) -> b
applyOperand w (val, chunks) = F.foldl' (applyChunk val) w chunks

-- | Place a chunk of @opVal@ into @w@ as defined by the chunk descriptor
--
-- The tuple describing the operand chunk is @(instructionIndex, operandIndex,
-- chunkSize)@; this function takes @chunkSize@ bits from @opVal@ starting at
-- @operandIndex@ and places those bits into @w@ at @instructionBit@.
applyChunk :: (Bits b, Num b) => b -> b -> (IBit, OBit, Word8) -> b
applyChunk opVal w (IBit instructionBit, OBit (fromIntegral -> operandBit), fromIntegral -> chunkSize) =
  w .|. (chunkBits `shiftL` instructionBit)
  where
    mask = (1 `shiftL` chunkSize) - 1
    chunkBits = mask .&. (opVal `shiftR` operandBit)
