{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Data.Word.Indexed ( W, w, unW, width ) where

import GHC.TypeLits
import Data.Bits
import qualified Text.PrettyPrint.HughesPJClass as PP

-- | Bit vectors of width @n@ with unsigned arithmetic ops.
--
-- Use 'w' or 'fromIntegral' to construct 'W' values, and use 'unW' to
-- destruct them.
newtype W (n :: Nat) = W { unW :: Integer } deriving (Eq,Ord)

-- | Smart constructor for 'W' values.
w :: KnownNat n => Integer -> W n
w = fromInteger

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

-- | Unsigned arithmetic, with a 2's complement representation of
-- "negative" values.
instance KnownNat n => Num (W n) where
  W x + W y     = fromInteger (x + y)
  W x * W y     = fromInteger (x * y)
  negate (W x)  = fromInteger (complement x + 1)
  abs           = id
  signum (W x)  = W (if x == 0 then 0 else 1)
  fromInteger n = res
    where res = W (nonNeg .&. (shiftL 1 (width res) - 1))
          nonNeg = if n < 0 then complement (abs n) + 1 else n

instance KnownNat n => PP.Pretty (W n) where
  pPrint = PP.integer . fromIntegral . unW
