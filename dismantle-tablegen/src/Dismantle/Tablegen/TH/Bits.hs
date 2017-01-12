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
import qualified Data.Sequence as Seq
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
assembleBits bitPattern operands = BS.pack (F.toList s2)
  where
    s0 = Seq.fromList (take (length bitPattern `div` 8) (repeat 0))
    s1 = foldr setExpectedBit s0 (zip [0..] bitPattern)
    s2 = foldr applyOperand s1 operands

setExpectedBit :: (Int, Bit) -> Seq.Seq Word8 -> Seq.Seq Word8
setExpectedBit (ix, bitVal) s =
  case bitVal of
    ExpectedBit True ->
      let (wordIx, bitIx) = ix `divMod` 8
      in Seq.adjust (`setBit` bitIx) wordIx s
    _ -> s

applyOperand :: OperandWrapper -> Seq.Seq Word8 -> Seq.Seq Word8
applyOperand (OperandWrapper val spec) s = foldr (setOperandBit val) s spec

setOperandBit :: (Bits b) => b -> (Int, Word8) -> Seq.Seq Word8 -> Seq.Seq Word8
setOperandBit val (insnIx, operandIx) s =
  case val `testBit` fromIntegral operandIx of
    False -> s
    True ->
      let (wordIx, bitIx) = insnIx `divMod` 8
      in Seq.adjust (`setBit` bitIx) wordIx s
