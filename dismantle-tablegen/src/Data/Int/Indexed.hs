{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Data.Int.Indexed ( I(..), width ) where

import GHC.TypeLits
import Data.Bits
import qualified Text.PrettyPrint.HughesPJClass as PP

newtype I (n :: Nat) = I { unI :: Int } deriving (Eq,Ord)

instance KnownNat n => Show (I n) where
  showsPrec _ me@(I n) = showChar '(' . shows n
                                      . showString " :: I "
                                      . shows (width me) . showChar ')'

instance KnownNat n => Bits (I n) where
  I x .&. I y       = I (x .&. y)
  I x .|. I y       = I (x .|. y)
  xor (I x) (I y)   = I (x `xor` y)
  complement (I x)  = fromIntegral (complement x)
  shiftL (I x) n    = fromIntegral (shiftL x n)
  shiftR (I x) n    = I (shiftR x n)
  bitSize           = width
  bitSizeMaybe      = Just . width
  testBit (I x) n   = testBit x n
  bit n             = I (bit n)
  isSigned _        = False
  popCount (I x)    = popCount x

  rotate me n = rotL me amt
    where
    amt | n >= 0    = if n < w then n else n'
        | otherwise = w + (if n > negate w then n else n')
    w               = width me
    n'              = mod n w

width :: KnownNat n => I n -> Int
width = fromIntegral . natVal

rotL :: KnownNat n => I n -> Int -> I n
rotL me@(I x) n =
  let shifted = shiftL x n
      upper   = fromIntegral shifted
      lower   = I (shiftR shifted (width me))
  in upper .|. lower

instance KnownNat n => Num (I n) where
  I x + I y     = fromIntegral (x + y)
  I x * I y     = fromIntegral (x * y)
  negate (I x)  = fromIntegral (negate x)
  abs           = id
  signum (I x)  = I (if x == 0 then 0 else 1)
  fromInteger n = res
    where res = I (fromIntegral n .&. (shiftL 1 (width res) - 1))

instance KnownNat n => PP.Pretty (I n) where
  pPrint = PP.integer . fromIntegral . unI
