{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Data.Word.Indexed ( W(..), width ) where

import GHC.TypeLits
import Data.Bits
import Numeric.Natural ( Natural )
import qualified Text.PrettyPrint.HughesPJClass as PP

newtype W (n :: Nat) = W { unW :: Natural } deriving (Eq,Ord)

instance KnownNat n => Show (W n) where
  showsPrec _ me@(W n) = showChar '(' . shows n
                                      . showString " :: W "
                                      . shows (width me) . showChar ')'

instance KnownNat n => Bits (W n) where
  W x .&. W y       = W (x .&. y)
  W x .|. W y       = W (x .|. y)
  xor (W x) (W y)   = W (x `xor` y)
  complement (W x)  = fromIntegral (complement x)
  shiftL (W x) n    = fromIntegral (shiftL x n)
  shiftR (W x) n    = W (shiftR x n)
  bitSize           = width
  bitSizeMaybe      = Just . width
  testBit (W x) n   = testBit x n
  bit n             = W (bit n)
  isSigned _        = False
  popCount (W x)    = popCount x

  rotate me n = rotL me amt
    where
    amt | n >= 0    = if n < w then n else n'
        | otherwise = w + (if n > negate w then n else n')
    w               = width me
    n'              = mod n w

width :: KnownNat n => W n -> Int
width = fromIntegral . natVal

rotL :: KnownNat n => W n -> Int -> W n
rotL me@(W x) n =
  let shifted = shiftL x n
      upper   = fromIntegral shifted
      lower   = W (shiftR shifted (width me))
  in upper .|. lower

instance KnownNat n => Num (W n) where
  W x + W y     = fromIntegral (x + y)
  W x * W y     = fromIntegral (x * y)
  negate (W x)  = fromIntegral (negate x)
  abs           = id
  signum (W x)  = W (if x == 0 then 0 else 1)
  fromInteger n = res
    where res = W (fromIntegral n .&. (shiftL 1 (width res) - 1))

instance KnownNat n => PP.Pretty (W n) where
  pPrint = PP.integer . fromIntegral . unW
