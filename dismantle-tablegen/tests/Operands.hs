{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Operands ( operandTests ) where

import GHC.TypeLits

import Control.Applicative
import Data.Bits
import qualified Data.Foldable as F
import Data.Proxy ( Proxy(..) )
import Data.Word ( Word8, Word32 )
import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as T

import Dismantle.Tablegen.Types (IBit(..))
import Dismantle.Tablegen.Parser.Types (OBit(..))
import Dismantle.Tablegen.TH.Bits

import Prelude

operandTests :: T.TestTree
operandTests = T.testGroup "Operands" [prop_roundtripChunking]

-- | Test to make sure that 'fieldFromWord' and 'assembleBits' roundtrip.
--
-- The test reads some randomly generated operands out of a randomly generated
-- word.  It then assembles those operands back into a word using the same
-- operand descriptions.  Note that the odd masking of @w@ in the final result
-- is to remove any bits that are not part of operands.  This is basically like
-- an instruction with all of its "required bits" set to zero.
prop_roundtripChunking :: T.TestTree
prop_roundtripChunking = T.testProperty "roundtripChunking" p
  where
    p :: (Word32, NonOverlappingChunks 32) -> Bool
    p (w, NOC chunks) =
      let mask = requiredBitMask chunks
          operands = [ (fieldFromWord w chunkDesc, chunkDesc)
                     | chunkDesc <- chunks
                     ]
      in assembleBits 0 operands == w .&. mask

data NonOverlappingChunks (n :: Nat) = NOC [[(IBit, OBit, Word8)]]
  deriving (Show)

instance (KnownNat n) => T.Arbitrary (NonOverlappingChunks n) where
  arbitrary = T.sized (mkNonOverlappingChunks (Proxy :: Proxy n))

mkNonOverlappingChunks :: (KnownNat n) => Proxy n -> Int -> T.Gen (NonOverlappingChunks n)
mkNonOverlappingChunks proxy (max 4 -> sz) = do
  nChunks <- T.choose (0, (max 4 sz))
  NOC <$> F.foldlM (genChunk nBits) [] [0..nChunks]
  where
    nBits = fromIntegral $ natVal proxy

genChunk :: Int -> [[(IBit, OBit, Word8)]] -> Int -> T.Gen [[(IBit, OBit, Word8)]]
genChunk nBits acc _chunkNum = do
  let highestBit = highestClaimedBitNumber acc
  case highestBit >= nBits of
    True -> return acc
    False -> do
      -- Add a gap between the last operand and this one
      gap <- T.choose (0, 2)
      contiguousChunk <- T.oneof [return True, return False]
      case contiguousChunk of
        True -> do
          chunkSize <- T.choose (2, 5)
          return ([(IBit (highestBit + gap), OBit 0, chunkSize)] : acc)
        False -> do
          -- Make a simple discontinuous chunk with a 1 bit gap between the
          -- (swapped) chunks.  Chunk 2 preceeds chunk1 in the instruction
          chunkSize1 <- T.choose (1, 3)
          chunkSize2 <- T.choose (1, 3)
          let start1 = highestBit + gap
              start2 = start1 + fromIntegral chunkSize1 + 1
          return ([ (IBit start1, OBit chunkSize2, chunkSize1)
                  , (IBit start2, OBit 0, fromIntegral chunkSize2)] : acc)


highestClaimedBitNumber :: [[(IBit, OBit, Word8)]] -> Int
highestClaimedBitNumber = F.foldl' goOp 0
  where
    goOp bitNo op = F.foldl' goChunk bitNo op
    goChunk bitNo (IBit ix, _, len) = max bitNo (ix + fromIntegral len)

-- | Compute a mask based on an operand description that has zeros for all of
-- the bits not covered by operands
requiredBitMask :: (Bits w, Num w) => [[(IBit, OBit, Word8)]] -> w
requiredBitMask = F.foldl' setOperandBits 0
  where
    setOperandBits = F.foldl' setChunkBits
    setChunkBits w (IBit i, _, sz) = go w i sz
    go w i sz
      | sz == 0 = w
      | otherwise = go (w `setBit` i) (i + 1) (sz - 1)
