{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dismantle.Arbitrary (
  Gen,
  Arbitrary(..),
  uniformR,
  withGen,
  createGen
  ) where

import GHC.TypeLits
import Data.Bits
import Data.Int ( Int16 )
import Data.Proxy ( Proxy(..) )
import Data.Word ( Word16 )

import qualified Data.Int.Indexed as I
import qualified Data.Word.Indexed as W
import qualified System.Random.MWC as R

newtype Gen = Gen R.GenIO

-- | This class allows for the generation of random values, similar to
-- QuickCheck.  The difference is that it uses a different random number
-- generator and doesn't require nearly as much infrastructure.
class Arbitrary a where
  arbitrary :: Gen -> IO a

uniformR :: (R.Variate a) => (a, a) -> Gen -> IO a
uniformR r (Gen g) = R.uniformR r g

instance forall n . (KnownNat n) => Arbitrary (W.W n) where
  arbitrary (Gen g) = W.W <$> R.uniformR (0, (1 `shiftL` nBits) - 1) g
    where
      nBits = fromIntegral (natVal (Proxy :: Proxy n))

instance forall n . (KnownNat n) => Arbitrary (I.I n) where
  arbitrary (Gen g) = I.I <$> R.uniformR (-maxVal, maxVal) g
    where
      nBits = fromIntegral (natVal (Proxy :: Proxy n))
      maxVal = ((1 `shiftL` nBits) - 1) `div` 2

instance Arbitrary Int16 where
  arbitrary (Gen g) = R.uniformR (minBound, maxBound :: Int16) g

instance Arbitrary Word16 where
  arbitrary (Gen g) = R.uniformR (0, maxBound :: Word16) g

withGen :: (Gen -> IO a) -> IO a
withGen k = R.withSystemRandom $ \g -> k (Gen g)

createGen :: IO Gen
createGen = Gen <$> R.createSystemRandom
