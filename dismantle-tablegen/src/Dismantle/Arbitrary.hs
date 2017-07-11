{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Dismantle.Arbitrary (
  Gen,
  Arbitrary(..),
  choose,
  uniform,
  uniformR,
  withGen,
  createGen
  ) where

import GHC.TypeLits
import Data.Bits
import Data.Int ( Int16 )
import Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import Data.Word ( Word16 )

import qualified Data.Int.Indexed as I
import qualified Data.Set.NonEmpty as NES
import qualified Data.Word.Indexed as W
import qualified System.Random.MWC as R

newtype Gen = Gen R.GenIO

-- | This class allows for the generation of random values, similar to
-- QuickCheck.  The difference is that it uses a different random number
-- generator and doesn't require nearly as much infrastructure.
class Arbitrary a where
  arbitrary :: Gen -> IO a

-- | Choose uniformly from a range, /inclusive/ of endpoints.
uniformR :: (R.Variate a) => (a, a) -> Gen -> IO a
uniformR r (Gen g) = R.uniformR r g

-- | Choose uniformly from a all elements of a type.
uniform :: (R.Variate a) => Gen -> IO a
uniform (Gen g) = R.uniform g

-- | Choose uniformly from a set.
choose :: (Ord a) => Gen -> NES.Set a -> IO a
choose gen (NES.flatten -> pool) = do
  ix <- uniformR (0, S.size pool - 1) gen
  return $ S.elemAt ix pool

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
