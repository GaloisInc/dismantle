{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Data.Word.Indexed ( W, w, wRep, unW, rep, width ) where

import           GHC.TypeLits
import           Data.Bits
import qualified Data.Parameterized.NatRepr as NR
import qualified Text.PrettyPrint.HughesPJClass as PP

-- | Bit vectors of width @n@ with unsigned arithmetic ops.
--
-- Use 'w' or 'fromIntegral' to construct 'W' values, and use 'unW' to
-- destruct them.
data W (n :: Nat) =
  W { unW :: Integer
    , rep :: NR.NatRepr n
    }
  deriving (Eq)

instance Ord (W n) where
  W w1 _ `compare` W w2 _ = compare w1 w2

-- | Smart constructor for 'W' values.
w :: KnownNat n => Integer -> W n
w i = W (fromInteger i) NR.knownNat

wRep :: NR.NatRepr n -> Integer -> W n
wRep r i = W (fromInteger i) r

instance Show (W n) where
  showsPrec _ me@(W n _) = showChar '(' . shows n
                                        . showString " :: W "
                                        . shows (width me) . showChar ')'

instance Bits (W n) where
  W x _ .&. W y r       = W (x .&. y) r
  W x _ .|. W y r       = W (x .|. y) r
  xor (W x _) (W y r)   = W (x `xor` y) r
  complement (W x r)  = safeW (complement x) r
  shiftL (W x r) n    = safeW (shiftL x n) r
  shiftR (W x r) n    = safeW (shiftR x n) r
  bitSize           = width
  bitSizeMaybe      = Just . width
  testBit (W x _) n   = testBit x n
  -- We can't pass in the repr here and we also can't add a KnownNat constraint
  -- on just this method, so we can't support it
  bit _n             = error "Size-indexed word type W does not support bit"
  isSigned _        = False
  popCount (W x _)    = popCount x

  rotate me n = rotL me amt
    where
    amt | n >= 0    = if n < w' then n else n'
        | otherwise = w' + (if n > negate w' then n else n')
    w'              = width me
    n'              = mod n w'

width :: W n -> Int
width = fromIntegral . NR.natValue . rep

rotL :: W n -> Int -> W n
rotL me@(W x r) n =
  let shifted = shiftL x n
      upper   = safeW shifted r
      lower   = safeW (shiftR shifted (width me)) r
  in upper .|. lower

-- | Unsigned arithmetic, with a 2's complement representation of
-- "negative" values.
instance KnownNat n => Num (W n) where
  W x _ + W y _    = fromInteger (x + y)
  W x _ * W y _     = fromInteger (x * y)
  negate (W x _r)  = fromInteger (complement x + 1)
  abs           = id
  signum (W x r)  = W (if x == 0 then 0 else 1) r
  fromInteger n = safeW n NR.knownNat

safeW :: Integer -> NR.NatRepr n -> W n
safeW n nr = W (nonNeg .&. (shiftL 1 (fromIntegral (NR.natValue nr)) - 1)) nr
  where
    nonNeg = if n < 0 then complement (abs n) + 1 else n

instance KnownNat n => PP.Pretty (W n) where
  pPrint = PP.integer . fromIntegral . unW
