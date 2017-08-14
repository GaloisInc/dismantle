{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Dismantle.Arbitrary (
  Gen,
  Arbitrary(..),
  categorical,
  categoricalChoose,
  choose,
  uniform,
  uniformR,
  withGen,
  createGen
  ) where

import GHC.TypeLits
import Control.Monad ( replicateM )
import Data.Bits
import qualified Data.Foldable as F
import Data.Int ( Int16 )
import Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import Data.Word ( Word16, Word64 )
import qualified Data.Vector.Unboxed as V

import qualified Data.Int.Indexed as I
import qualified Data.Set.NonEmpty as NES
import qualified Data.Word.Indexed as W
import qualified System.Random.MWC as R
import qualified System.Random.MWC.Distributions as R

newtype Gen = Gen R.GenIO

-- | This class allows for the generation of random values, similar to
-- QuickCheck.  The difference is that it uses a different random number
-- generator and doesn't require nearly as much infrastructure.
class Arbitrary a where
  arbitrary :: Gen -> IO a

-- | Choose uniformly from a range, /inclusive/ of endpoints.
uniformR :: (R.Variate a) => (a, a) -> Gen -> IO a
uniformR r (Gen g) = R.uniformR r g

-- | Choose uniformly from a *fixed subset* of a type.
--
-- For fixed-width integral types the whole type is used.
-- For floating point numbers the range @(0,1]@ is used.
uniform :: (R.Variate a) => Gen -> IO a
uniform (Gen g) = R.uniform g

-- | Choose uniformly from a non-empty set.
choose :: NES.Set a -> Gen -> IO a
choose (NES.view -> (a, pool)) gen = do
  ix <- uniformR (0, S.size pool) gen
  let isOutsidePool = ix == S.size pool
  return $ if isOutsidePool then a else S.elemAt ix pool

-- | Choose an index based on the given *relative* non-negative
-- weights.
--
-- E.g. @categorical [1/6, 1/3, 1/2]@ will return 0 with probability
-- 1/6, 1 with probability 1/3, and 2 with probability 1/2.
--
-- E.g. @categorical [1, 2]@ will return 0 with probability 1/3
-- and 1 with probability 2/3.
categorical :: [Double] -> Gen -> IO Int
categorical weights (Gen g) = R.categorical (V.fromList weights) g

-- | A version of 'categorical' that chooses a weighted element.
--
-- In term of 'categoricalChoose' we have
--
-- > categorical weights = categoricalChoose (zip weights [0..])
categoricalChoose :: [(Double, a)] -> Gen -> IO a
categoricalChoose weightedElems gen = do
  let (weights, elems) = unzip weightedElems
  index <- categorical weights gen
  return $ elems !! index

instance forall n . (KnownNat n) => Arbitrary (W.W n) where
  arbitrary (Gen g) = do
    chunks :: [Word64]
           <- replicateM nChunks (R.uniform g)
    let (n, _) = F.foldl' shiftOr (0, 0) chunks
    return (W.w (n .&. mask))
    where
      nBits = fromIntegral (natVal (Proxy :: Proxy n))
      mask = (1 `shiftL` nBits) - 1
      (nWords, leftoverBits) = nBits `divMod` 64
      nChunks = nWords + if leftoverBits == 0 then 0 else 1
      shiftOr (n, ix) c = (n .|. (fromIntegral c `shiftL` (ix * 64)), ix + 1)

instance forall n . (KnownNat n) => Arbitrary (I.I n) where
  arbitrary (Gen g) = I.I <$> R.uniformR (-maxVal - 1, maxVal) g
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
