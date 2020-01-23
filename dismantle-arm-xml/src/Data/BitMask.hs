{-# Language GADTs, DataKinds, TypeOperators, BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | An abstraction of 'ByteTrie' bits that allows abstract types to be considered "bit-like", with
-- corresponding container datatypes for constructing masks out of them.
module Data.BitMask
  ( IsMaskBit(..)
  , asBits
  , equivBit
  , equivMasks
  , matchesBit
  , matchesMask
  , BitSection(..)
  , mkBitSection
  , QuasiBit(..)
  , BitMask
  , computePattern'
  , computePattern
  , readBit
  , readQuasiBit
  , deriveMasks
  , prettyMask
  , prettyMask'
  , V.toList
  )
  where

import           GHC.TypeNats
import qualified Control.Monad.Except as ME
import           Control.Monad ( unless, zipWithM )
import           Data.Maybe ( fromMaybe )
import           Data.List ( intercalate, nub )
import           Data.List.Split as LS
import           Data.Type.Equality
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Vector as V
import           Data.Parameterized.NatRepr ( type (<=), type (+), NatRepr )
import qualified Data.Parameterized.NatRepr as NR

import qualified Text.PrettyPrint.HughesPJClass as PP

import qualified Dismantle.Tablegen.ByteTrie as BT

import           Data.PropTree ( PropTree )
import qualified Data.PropTree as PropTree

pattern BitSet = BT.ExpectedBit True
pattern BitUnset = BT.ExpectedBit False
pattern BitAny = BT.Any

class IsMaskBit a where
  asBit :: a -> BT.Bit
  -- | asBit (anyMaskBit a) == BT.Any
  anyMaskBit :: a

  showBit :: a -> String
  showBit a = case asBit a of
    BitSet -> "1"
    BitUnset -> "0"
    BitAny -> "x"

  mergeBits :: a -> a -> Maybe a
  mergeBits a1 a2 = case (asBit a1, asBit a2) of
    (BT.ExpectedBit x, BT.ExpectedBit y) | x == y -> return a1
    (BT.Any, _) -> return a2
    (_, BT.Any) -> return a1
    _ -> fail "Incompatible bits"

instance IsMaskBit BT.Bit where
  asBit bit = bit
  anyMaskBit = BT.Any

readBit :: String -> Maybe BT.Bit
readBit s = case s of
  "1" -> Just $ BitSet
  "0" -> Just $ BitUnset
  "x" -> Just $ BitAny
  "" -> Just $ BitAny
  _ -> Nothing

instance IsMaskBit a => IsMaskBit (Maybe a) where
  asBit mbit = case mbit of
    Nothing -> BT.Any
    Just bit -> asBit bit
  anyMaskBit = Nothing
  showBit mbit = case mbit of
    Nothing -> "?"
    Just bit -> showBit bit
  mergeBits mb1 mb2 = case (mb1, mb2) of
    (Just b1, Just b2) -> Just $ mergeBits b1 b2
    (_, Nothing) -> Just $ mb1
    (Nothing, _) -> Just $ mb2

-- | A mask bit with two additional states, representing a relaxed constraint.
data QuasiBit =
    Bit BT.Bit
    -- ^ a normal bittrie specifying a bitmask bit
  | QBit Bool
    -- ^ a qbit (quasi-bit) specifies that a given bitpattern is "required" to have defined
    -- behavior, but not to disambiguate instructions.
  deriving (Eq, Ord, Show)

instance IsMaskBit QuasiBit where
  asBit qbit = case qbit of
    Bit bit -> bit
    QBit _ -> BT.Any

  showBit qb = case qb of
    Bit b -> showBit b
    QBit True -> "I"
    QBit False -> "O"

  anyMaskBit = Bit BT.Any

  mergeBits qb1 qb2 = case (qb1, qb2) of
    (QBit x, QBit y) | x == y -> return $ QBit y
    (QBit x, Bit (BT.ExpectedBit y)) | x == y -> return $ Bit $ BT.ExpectedBit y
    (Bit (BT.ExpectedBit y), QBit x) | x == y -> return $ Bit $ BT.ExpectedBit y
    (_, Bit BT.Any) -> return qb1
    (Bit BT.Any, _) -> return qb2
    (Bit b1, Bit b2) -> Bit <$> mergeBits b1 b2
    _ -> fail "Incompatible qbits"

instance PP.Pretty QuasiBit where
  pPrint qbit = PP.text (showBit qbit)

readQuasiBit :: String -> Maybe QuasiBit
readQuasiBit s = case s of
  "1" -> Just $ Bit $ BitSet
  "0" -> Just $ Bit $ BitUnset
  "x" -> Just $ Bit $ BitAny
  "" -> Just $ Bit $ BitAny
  "(1)" -> Just $ QBit True
  "(0)" -> Just $ QBit $ False
  _ -> Nothing

asBits :: IsMaskBit a => [a] -> [BT.Bit]
asBits = map asBit

equivBit :: IsMaskBit a => a -> a -> Bool
equivBit bit bit' = case (asBit bit, asBit bit') of
  (BitSet, BitSet) -> True
  (BitUnset, BitUnset) -> True
  (BitAny, BitAny) -> True
  _ -> False

-- | Does the first bit match against the second bit.
-- i.e. is the first bit at least as general as the second
matchesBit :: IsMaskBit a => a -> a -> Bool
matchesBit bit bit' = case asBit bit' of
  BitAny -> True
  _ -> equivBit bit bit'

type BitMask n a = V.Vector n a

defaultBitMask :: IsMaskBit a => (1 <= n) => NatRepr n -> BitMask n a
defaultBitMask nr
  | NR.Refl <- NR.minusPlusCancel nr (NR.knownNat @1)
  = V.generate (NR.decNat nr) (\_ -> anyMaskBit)

prettyMask :: forall a n. IsMaskBit a => BitMask n a -> PP.Doc
prettyMask = prettyMask' id

prettyMask' :: forall a n. IsMaskBit a => ([a] -> [a]) -> BitMask n a -> PP.Doc
prettyMask' endianness mask = PP.hcat $ PP.punctuate (PP.text ".") $ (map PP.hcat $ LS.chunksOf 8 (map go bits))
  where
    bits = endianness $ V.toList mask

    go :: a -> PP.Doc
    go a = PP.text (showBit a)

equivMasks :: IsMaskBit a => BitMask n a -> BitMask n a -> Bool
equivMasks v v' = all id (V.zipWith equivBit v v')

matchesMask :: IsMaskBit a => BitMask n a -> BitMask n a -> Bool
matchesMask v v' = all id (V.zipWith matchesBit v v')


-- | A slice of bits at a given bit position (0-indexed starting at the head of a list of bits).
-- The 'n' type parameter represents the size of bitmasks that this is compatible with.
-- e.g. all 'BitSection's for a 32-bit architecture will be a 'BitSection a 32' regardless of their actual
-- widths, which are constrained to fit in the given bitwidth.
data BitSection n a where
  BitSection :: forall a n posAt sectWidth
              . (posAt + 1 <= n, 1 <= sectWidth, sectWidth <= n, posAt + sectWidth <= n)
             => NatRepr posAt
             -> BitMask sectWidth a
             -> BitSection n a

deriving instance Functor (BitSection n)
deriving instance Foldable (BitSection n)

instance Eq a => Eq (BitSection n a) where
  (BitSection posAt vect) == (BitSection posAt' vect')
    | Just Refl <- testEquality posAt posAt'
    , Just Refl <- testEquality (V.length vect) (V.length vect')
      = vect == vect'
    | otherwise = False

instance IsMaskBit a => Show (BitSection n a) where
  show bitsect = showBitSection bitsect

instance (KnownNat n, PP.Pretty a) => PP.Pretty (BitSection n a) where
  pPrint bitsect = prettyBitSectionHiBit NR.knownNat PP.pPrint bitsect

mkBitSection :: IsMaskBit a => Int -> [a] -> NatRepr n -> Maybe (BitSection n a)
mkBitSection posInt bits nr
  | Just (Some bitLen) <- NR.someNat (length bits)
  , Just NR.LeqProof <- NR.testLeq (NR.knownNat @1) bitLen
  , Just (Some posRepr) <- NR.someNat posInt
  , Just mask <- V.fromList bitLen bits
  , Just NR.LeqProof <- NR.testLeq (NR.incNat posRepr) nr
  , Just NR.LeqProof <- NR.testLeq (V.length mask) nr
  , Just NR.LeqProof <- NR.testLeq (posRepr `NR.addNat` (V.length mask)) nr
    = Just $ BitSection posRepr mask
  | otherwise = Nothing

sectBitPos :: BitSection n a -> Int
sectBitPos (BitSection posAt _)  = fromIntegral $ NR.intValue posAt

sectHiBitPos :: BitSection n a -> NatRepr n -> Int
sectHiBitPos (BitSection posAt _) nr = fromIntegral $ (NR.intValue nr - NR.intValue posAt - 1)

sectBits :: BitSection n a -> [a]
sectBits (BitSection _ mask) = V.toList mask

sectWidth :: BitSection n a -> Int
sectWidth (BitSection _ mask) = V.lengthInt mask

prettyBitSectionHiBit :: NatRepr n -> (a -> PP.Doc) -> BitSection n a -> PP.Doc
prettyBitSectionHiBit nr prettyBit bitsect =
  let hiBit = sectHiBitPos bitsect nr in
  (PP.hcat $ map prettyBit (sectBits bitsect))
  PP.<> PP.text "<"
  PP.<> case sectWidth bitsect of
    1 -> PP.int hiBit
    x | x > 1 -> PP.int hiBit PP.<> PP.text ":" PP.<> PP.int (hiBit - x + 1)
    _ -> PP.text "?"
  PP.<> PP.text ">"

prettyBitSection :: (a -> PP.Doc) -> BitSection n a -> PP.Doc
prettyBitSection prettyBit bitsect =
  (PP.hcat $ map prettyBit (sectBits bitsect))
  PP.<> PP.text "<"
  PP.<> case sectWidth bitsect of
    1 -> PP.int (sectBitPos bitsect)
    x | x > 1 -> PP.int ((x - 1) + (sectBitPos bitsect)) PP.<> PP.text "+:" PP.<> PP.int (sectBitPos bitsect)
    _ -> PP.text "?"
  PP.<> PP.text ">"

showBitSection :: IsMaskBit a => BitSection n a -> String
showBitSection bitsect = PP.render $ prettyBitSection (PP.text . showBit) bitsect

mergeBitErr :: ME.MonadError String m => IsMaskBit a => a -> a -> m a
mergeBitErr a1 a2 = case mergeBits a1 a2 of
  Just a -> return a
  Nothing -> ME.throwError $ "incompatible bits: " ++ showBit a1 ++ " vs. " ++ showBit a2

addSectionToMask :: ME.MonadError String m => IsMaskBit a => BitSection n a -> BitMask n a -> m (BitMask n a)
addSectionToMask (BitSection posAt src) dest = do
  let destSlice = V.slice posAt (V.length src) dest
  merged <- V.zipWithM mergeBitErr src destSlice
  return $ V.replace posAt merged dest

prependErr :: ME.MonadError String m => String -> String -> m a
prependErr msg err = ME.throwError $ msg ++ " " ++ err

-- | Same as 'computePattern', however bits not set in any given 'BitSection' are left as 'Nothing'
computePattern' :: forall a m n
                 . (ME.MonadError String m, 1 <= n)
                => IsMaskBit a
                => NatRepr n
                -> [BitSection n a]
                -> m (BitMask n (Maybe a))
computePattern' nr bitsects =
  go bitsects (defaultBitMask nr)
    `ME.catchError`
     (prependErr $ "computePattern: " ++ intercalate "," (map showBitSection bitsects))
  where
    go :: [BitSection n a] -> BitMask n (Maybe a) -> m (BitMask n (Maybe a))
    go [] mask = return $ mask
    go (bitsect : rst) mask = do
      resultMask <- addSectionToMask (fmap Just bitsect) mask
        `ME.catchError`
        (prependErr $ "computePattern: for BitSection: " ++ showBitSection bitsect)
      go rst resultMask

-- | Flattens a 'BitSection' list into a single list of elements. Overlapping sections are
-- merged according to 'mergeBits' from 'IsMaskBit', with merge failure (due to incompatible bits)
-- throwing an exception in the given error monad.
computePattern :: (IsMaskBit a, ME.MonadError String m, 1 <= n) => NatRepr n -> [BitSection n a] -> m (BitMask n a)
computePattern nr bitsects = fmap (fromMaybe anyMaskBit) <$> computePattern' nr bitsects

-- This bit explosion is actually redundant

-- explodeAny :: forall a n. IsMaskBit a => 1 <= n => BitMask n a -> [BitMask n BT.Bit]
-- explodeAny mask = case V.uncons mask of
--   (b, Left NR.Refl) -> do
--     b' <- explodeBit b
--     return $ V.singleton b'
--   (b, Right rst) -> do
--     b' <- explodeBit b
--     NR.LeqProof <- return $ V.nonEmpty rst
--     mask' <- explodeAny rst
--     NR.Refl <- return $ NR.minusPlusCancel (V.length mask) (NR.knownNat @1)
--     return $ V.cons b' mask'
--   where
--     explodeBit :: a -> [BT.Bit]
--     explodeBit b = case asBit b of
--       BT.Any -> [BT.ExpectedBit True, BT.ExpectedBit False]
--       b' -> [b']

-- explodeBitSection :: forall a n. IsMaskBit a => 1 <= n => BitSection n a -> [BitSection n BT.Bit]
-- explodeBitSection (BitSection posAt mask) = [ (BitSection posAt mask') | mask' <- explodeAny mask ]


-- | Derive a set of positive and negative masks from a given 'PropTree' of 'BitSection'.
-- e.g. turn ( x1x<0:2> && 11 <2:4> && !(010<0:2>) && !(11x<0:2>) into
--           ([x1x11], [ [010xx], [11xxx] ])
deriveMasks :: forall a m n
             . (IsMaskBit a, ME.MonadError String m, 1 <= n)
            => NatRepr n
            -> PropTree (BitSection n a)
            -> m (BitMask n a, [BitMask n a])
deriveMasks nr constraints = case PropTree.toConjunctsAndDisjuncts constraints of
  Just (positiveConstraints, negativeConstraints) -> do
    mask' <- computePattern nr positiveConstraints
             `ME.catchError`
             (prependErr $ "deriveMasks: invalid positive constraint")

    negMasks <- sequence $ do
      negConstraintBase <- negativeConstraints
      return $ computePattern nr negConstraintBase
         `ME.catchError`
         (prependErr $ "deriveMasks: invalid negative constraint")
    return (mask', negMasks)
  Nothing -> ME.throwError $
    "Malformed PropTree for mask derivation: \n"
    ++ PP.render (PropTree.prettyPropTree (PP.text . showBitSection) constraints)
