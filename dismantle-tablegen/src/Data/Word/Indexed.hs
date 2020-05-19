{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.Word.Indexed ( W, w, wRep, unW, rep, width ) where

import           Data.Bits
import qualified Data.Parameterized.NatRepr as NR
import           GHC.TypeLits
import qualified Text.PrettyPrint.HughesPJClass as PP


-- | Bit vectors of width @n@ with unsigned arithmetic ops.
--
-- Use 'w' or 'fromIntegral' to construct 'W' values, and use 'unW' to
-- destruct them.
--
-- Note that using 'w' will mask the input value to the allowed number
-- of bits, whereas 'fromIntegral' will generate an error exception.
-- This is partially to prevent implicit suprises; for example, Eq
-- testing will implicitly use fromIntegral, so '47 == (1 :: W 1)'
-- would be true and probably be surprising or a hidden problem.
data W (n :: Nat) =
  W { unW :: Integer
    , rep :: NR.NatRepr n
    }
  deriving (Eq)

instance Ord (W n) where
  W w1 _ `compare` W w2 _ = compare w1 w2

-- | Smart constructor for 'W' values.  The initialization will ensure
-- that the 'W' result value is constrained by the number of available
-- bits; any excess bits from the initialization value will be
-- (silently) discarded.
w :: KnownNat n => Integer -> W n
w i = safeW i NR.knownNat

-- | Smart constructor for 'W' values where the size is explicitly
-- provided and type-parameterized.  As with the 'w' constructor, any
-- excess bits in the input value will be discarded and the result
-- value will utilize only the available bits.
wRep :: NR.NatRepr n -> Integer -> W n
wRep = flip safeW

instance Show (W n) where
  showsPrec _ me@(W n _) = showChar '(' . shows n
                                        . showString " :: W "
                                        . shows (width me) . showChar ')'

instance KnownNat n => Enum (W n) where
    toEnum = w . toInteger
    fromEnum = fromInteger . unW

instance KnownNat n => Bounded (W n) where
    maxBound = w (toInteger (maxBound :: Int))
    minBound = w 0

-- | Perform standard bit-wise operations.  All operations preserve
-- the size, and require both arguments to have the same size.
instance Bits (W n) where
  W x _ .&. W y r       = W (x .&. y) r  -- n.b. x has rep value of r also
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
width = NR.widthVal . rep

rotL :: W n -> Int -> W n
rotL me@(W x r) n =
  let shifted = shiftL x n
      upper   = safeW shifted r
      lower   = safeW (shiftR shifted (width me)) r
  in upper .|. lower

-- | Unsigned arithmetic, with a 2's complement representation of
-- "negative" values.
instance KnownNat n => Num (W n) where
  W x _ + W y _   = w (x + y)
  W x _ * W y _   = w (x * y)
  negate (W x _r) = w (complement x + 1)
  abs             = id
  signum (W x r)  = W (if x == 0 then 0 else 1) r

  -- The 'fromInteger' implementation will generate an error if the
  -- input value is too large for this W to represent.  This is
  -- because 'fromInteger' is often implicitly called and therefore
  -- this error avoids misleading results like '(47 == (1 :: W 1)) ==
  -- True'.  To create a 'W' value while discarding excess bits, use
  -- the 'w' or 'wRep' explicit constructors.
  fromInteger n   = let kn = NR.knownNat in
                    if n > NR.maxUnsigned kn || n < NR.minUnsigned kn
                    then error ("Value " <> show n <>
                                " too large for Word.Indexed of size " <>
                                (show $ NR.natValue kn))
                    else safeW n kn

instance KnownNat n => Real (W n) where
    toRational (W x _) = toRational x

instance KnownNat n => Integral (W n) where
    quotRem (W x _) (W y _) = let (x', y') = quotRem x y in (w x', w y')
    toInteger = unW

-- safeW is a safe initializer that will automatically truncate the
-- value to the allowed word size, dropping any excess.  It is "safe"
-- in the sense that it will never cause a runtime error.
safeW :: Integer -> NR.NatRepr n -> W n
safeW n nr = W (NR.toUnsigned nr nonNeg) nr
  where
    nonNeg = if n < 0 then complement (abs n) + 1 else n

instance KnownNat n => PP.Pretty (W n) where
  pPrint = PP.integer . fromIntegral . unW
