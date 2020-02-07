{-# Language GADTs, DataKinds, TypeOperators, BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}

-- | An abstraction of 'ByteTrie' bits that allows abstract types to be considered "bit-like", with
-- corresponding container datatypes for constructing masks out of them.
module Data.BitMask
  ( MaskBit(..)
  , SemiMaskBit(..)
  , HasBottomMaskBit(..)
  , WithBottom(..)
  , ShowableBit(..)
  , BitSection
  , maskAsBitSection
  , sectionOfConstraint
  , sectTotalSetWidth
  , prettyBitSection
  , AsBit(..)
  , asContiguousSections
  , mergeBitM
  , mergeBitErr
  , mkBitSection
  , mkBitSectionHiBit
  , bitSectionFromList
  , bitSectionFromListHiBit
  , QuasiBit
  , mkQuasiBit
  , unQuasiBit
  , BitMask
  , splitBitMask
  , bottomBitMask
  , mergeBitMasksErr
  , SomeBitMask(..)
  , someBitMaskFromCons
  , computePattern
  , readBit
  , readQuasiBit
  , flattenQuasiBit
  , isQBit
  , bitToQuasi
  , bitAsQuasi
  , deriveMasks
  , prettyMask
  , prettySegmentedMask
  , V.toList
  , V.fromList
  , V.lengthInt
  , V.length
  , MaskTrie
  , emptyMaskTrie
  , addToMaskTrie
  , lookupMaskTrie
  )
  where

import           Prelude hiding ( zipWith, length )

import           GHC.TypeNats
import           Control.Monad.Identity ( runIdentity )
import qualified Control.Monad.Except as ME
import           Control.Monad ( unless, foldM )
import           Data.Maybe ( fromMaybe, catMaybes, isJust, fromJust )
import           Data.List ( intercalate, nub )
import qualified Data.List as List
import           Data.List.Split as LS
import           Data.Type.Equality
import           Data.Void
import           Data.Parameterized.TraversableFC
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

-- | This class generalizes the notion of bit-like matching. Algebraically, this
-- class defines a lattice, where 'Nothing' is the top element and merging of bits is join.
class Eq a => SemiMaskBit a where
  -- | Bit merging is a commutative binary operation (lattice join),
  -- where the result is the least general bit that would match the source bits.
  -- 'Nothing' is the distinguished element representing an unmatchable bit.
  -- This is defined as a partial operation since in practice
  -- we only want to consider masks which do not contain 'Nothing'.
  --
  -- For example: 'x `mergeBit` 1 = Just 1', '1 `mergeBit` 0 = Nothing'
  mergeBit :: a -> a -> Maybe a

  -- | Bit merging induces the standard lattice partial ordering, i.e.:
  -- 'a1 `leqBit` a2' iff 'a1 `mergeBit` a2 = Just a2'
  --
  -- For example: 'x <= 1', 'x <= 0', 'x <= x', '1 <= 1', '0 <= 0', but not: '1 <= x', nor '0 <= x', nor '0 <= 1'
  leqBit :: a -> a -> Bool
  a1 `leqBit` a2 = case mergeBit a1 a2 of
    Just a3 -> a2 == a3
    Nothing -> False

  -- | Bit merging induces an equivalence class of comparable bits, i.e.:
  -- 'a1 `matchBit` a2' iff 'a1 `leqBit a1` || a2 `leqBit` a1'
  -- or equivalently:
  -- 'a1 `matchBit` a2' iff 'a1 `matchBit` a2 /= Nothing'
  --
  -- For example: 'x =~ 1', '1 =~ x', '1 =~ 1', but not: '1 =~ 0'
  matchBit :: a -> a -> Bool
  a1 `matchBit` a2 = isJust (a1 `mergeBit` a2)

-- | Defines the unit element (wildcard) of a bit-like type, which is the bottom
-- of the lattice.
class SemiMaskBit a => HasBottomMaskBit a where
  -- | The unit element for bit merging.
  --
  -- For any element 'a' the following identity must hold:
  -- 'bottomBit `mergeBit` a = Just a'
  -- Which is equivalent to the following identities. For any 'a':
  -- 'bottomBit `leqBit` a'
  -- 'bottomBit `matchBit` a'
  --
  -- For example: 'bottomBit :: BT.Bit = BT.Any'
  bottomBit :: a

class ShowableBit a where
  -- | Display an element according to its bit-semantics.
  -- For example, for 'BT.Bit' we have:
  -- 'BT.Any' -> 'x'
  -- 'BT.ExpectedBit True' -> '1'
  -- 'BT.ExpectedBit False' -> '0'
  showBit :: a -> String

type MaskBit a = (HasBottomMaskBit a, ShowableBit a)

mergeBitM :: ME.MonadError (a, a) m => SemiMaskBit a => a -> a -> m a
mergeBitM a1 a2 = case mergeBit a1 a2 of
  Just a -> return a
  Nothing -> ME.throwError (a1, a2)

mergeBitErr :: ME.MonadError String m => SemiMaskBit bit => ShowableBit bit => bit -> bit -> m bit
mergeBitErr m1 m2 = case mergeBitM m1 m2 of
  Left (a1, a2) -> ME.throwError $ "incompatible bits: " ++ showBit a1 ++ " " ++ showBit a2
  Right a -> return a

instance SemiMaskBit BT.Bit where
  a1 `mergeBit` a2 = case (a1, a2) of
     (BT.ExpectedBit x, BT.ExpectedBit y) | x == y -> return a1
     (BT.Any, _) -> return a2
     (_, BT.Any) -> return a1
     _ -> fail "Incompatible bits"

instance ShowableBit BT.Bit where
  showBit a = case a of
    BitSet -> "1"
    BitUnset -> "0"
    BitAny -> "x"

instance HasBottomMaskBit BT.Bit where
  bottomBit = BT.Any

instance SemiMaskBit Bool where
  b1 `mergeBit` b2 = if b1 == b2 then Just b1 else Nothing

instance ShowableBit Bool where
  showBit b = case b of
      True -> "1"
      False -> "0"

instance SemiMaskBit () where
  _ `mergeBit` _ = Just ()

instance HasBottomMaskBit () where
  bottomBit = ()

instance ShowableBit () where
  showBit _ = "_"

readBit :: String -> Maybe BT.Bit
readBit s = case s of
  "1" -> Just $ BitSet
  "0" -> Just $ BitUnset
  "x" -> Just $ BitAny
  "" -> Just $ BitAny
  _ -> Nothing

-- | Add a distinguished bottom bit to any 'SemiMaskBit' (i.e. the least specific/most general bit).
-- Structurally equivalent to 'Maybe' but with explicit bit-semantics.
data WithBottom a = BottomBit | JustBit a
  deriving (Eq, Show, Ord, Foldable, Functor, Traversable)

instance SemiMaskBit a => SemiMaskBit (WithBottom a) where
  mb1 `mergeBit` mb2 = case (mb1, mb2) of
    (JustBit b1, JustBit b2) -> JustBit <$> mergeBit b1 b2
    (_, BottomBit) -> Just $ mb1
    (BottomBit, _) -> Just $ mb2

  mb1 `leqBit` mb2 = case (mb1, mb2) of
    (JustBit b1, JustBit b2) -> b1 `leqBit` b2
    (BottomBit, _) -> True
    _ -> False

instance ShowableBit a => ShowableBit (WithBottom a) where
  showBit mb = case mb of
    BottomBit -> "?"
    JustBit b -> showBit b

instance SemiMaskBit a => HasBottomMaskBit (WithBottom a) where
  bottomBit = BottomBit

instance (SemiMaskBit a, SemiMaskBit b) => SemiMaskBit (Either a b) where
  ab1 `mergeBit` ab2 = case (ab1, ab2) of
    (Left a1, Left a2) -> Left <$> a1 `mergeBit` a2
    (Right b1, Right b2) -> Right <$> b1 `mergeBit` b2
    _ -> Nothing

  ab1 `leqBit` ab2 = case (ab1, ab2) of
    (Left a1, Left a2) -> a1 `leqBit` a2
    (Right b1, Right b2) -> b1 `leqBit` b2
    _ -> False

  ab1 `matchBit` ab2 = case (ab1, ab2) of
    (Left a1, Left a2) -> a1 `matchBit` a2
    (Right b1, Right b2) -> b1 `matchBit` b2
    _ -> False

instance (ShowableBit a, ShowableBit b) => ShowableBit (Either a b) where
  showBit ab = case ab of
    Left a -> showBit a
    Right b -> showBit b

instance (SemiMaskBit a, SemiMaskBit b) => SemiMaskBit (a, b) where
  (a1, b1) `mergeBit` (a2, b2) = do
    a <- a1 `mergeBit` a2
    b <- b1 `mergeBit` b2
    return $ (a, b)

  (a1, b1) `leqBit` (a2, b2) = a1 `leqBit` a2 && b1 `leqBit` b2
  (a1, b1) `matchBit` (a2, b2) = a1 `matchBit` a2 && b1 `matchBit` b2

instance (HasBottomMaskBit a, HasBottomMaskBit b) => HasBottomMaskBit (a, b) where
  bottomBit = (bottomBit, bottomBit)

-- | A simple wrapper around any equality type that gives it bit-like semantics.
newtype AsBit a = AsBit a
  deriving (Eq, Ord, Show)

instance Eq a => SemiMaskBit (AsBit a) where
  a1 `mergeBit` a2 = if a1 == a2 then Just a1 else Nothing
  a1 `leqBit` a2 = a1 == a2
  a1 `matchBit` a2 = a1 == a2

-- | A quasi-bit is a tagged bit indicating that a mask element is a "soft" requirement.
-- Dropping a 'QuasiBit' to a 'BT.Bit' will convert any tagged bits into wildcards.
newtype QuasiBit = QuasiBit (BT.Bit, WithBottom ())
  deriving (Eq, Ord, Show, SemiMaskBit, HasBottomMaskBit)

instance ShowableBit QuasiBit where
  showBit (QuasiBit (bit, mquasi)) = case (bit, mquasi == JustBit ()) of
    (BT.ExpectedBit True, True) -> "I"
    (BT.ExpectedBit False, True) -> "O"
    (BT.ExpectedBit True, False) -> "1"
    (BT.ExpectedBit False, False) -> "0"
    (BT.Any, True) -> "X"
    (BT.Any, False) -> "x"


instance PP.Pretty QuasiBit where
  pPrint bit = PP.text (showBit bit)

mkQuasiBit :: (BT.Bit, Bool) -> QuasiBit
mkQuasiBit (bit, isQuasi) = QuasiBit (bit, if isQuasi then JustBit () else BottomBit)

unQuasiBit :: QuasiBit -> (BT.Bit, Bool)
unQuasiBit (QuasiBit (bit, qb)) = (bit, qb == JustBit ())

-- | Make a 'QuasiBit' from a 'BT.Bit'. If the first argument is 'True' then the bit is marked
-- as a soft requirement that can be flattened into a wildcard when constructing a mask.
bitToQuasi :: Bool -> BT.Bit -> QuasiBit
bitToQuasi isQuasi bit = QuasiBit (bit, if isQuasi then JustBit () else BottomBit)

-- | Lift a 'BT.Bit' to a 'QuasiBit', leaving it as a hard requirement.
bitAsQuasi :: BT.Bit -> QuasiBit
bitAsQuasi = bitToQuasi False

-- | Retrieve the inner 'BT.Bit' from a 'QuasiBit', ignoring its status as either a hard or soft requirement.
getBitFromQuasi :: QuasiBit -> BT.Bit
getBitFromQuasi (QuasiBit (bit, _)) = bit

-- | Return whether or not a 'QuasiBit' has been marked as a soft requirement
-- i.e. (is a "QBit" rather than a true "Bit").
isQBit :: QuasiBit -> Bool
isQBit (QuasiBit (_, JustBit _)) = True
isQBit _ = False

-- | Flatten a 'QuasiBit' to a 'BT.Bit' by dropping any soft requirements.
flattenQuasiBit :: QuasiBit -> BT.Bit
flattenQuasiBit qbit = if isQBit qbit then BT.Any else getBitFromQuasi qbit

readQuasiBit :: String -> Maybe QuasiBit
readQuasiBit s = case s of
  "1" -> Just $ bitToQuasi False BitSet
  "0" -> Just $ bitToQuasi False BitUnset
  "x" -> Just $ bitToQuasi False BitAny
  "" -> Just $ bitToQuasi False BitAny
  "(1)" -> Just $ bitToQuasi True BitSet
  "(0)" -> Just $ bitToQuasi True BitUnset
  _ -> Nothing

type BitMask n bit = V.Vector n bit

instance SemiMaskBit bit => SemiMaskBit (BitMask n bit) where
  m1 `mergeBit` m2 = V.zipWithM mergeBit m1 m2
  m1 `matchBit` m2 = all id (V.zipWith matchBit m1 m2)
  m1 `leqBit` m2 = all id (V.zipWith leqBit m1 m2)

instance ShowableBit bit => ShowableBit (BitMask n bit) where
  showBit m = PP.render (prettyMask m)

instance (HasBottomMaskBit bit, KnownNat n, 1 <= n) => HasBottomMaskBit (BitMask n bit) where
  bottomBit = bottomBitMask NR.knownNat

bottomBitMask :: forall bitmask n bit. 1 <= n => HasBottomMaskBit bit => NR.NatRepr n -> BitMask n bit
bottomBitMask nr
  | NR.Refl <- NR.minusPlusCancel nr (NR.knownNat @1)
  = V.generate (NR.decNat nr) (\_ -> bottomBit)

mergeBitMasksM :: ME.MonadError (bit, bit) m => SemiMaskBit bit => BitMask n bit -> BitMask n bit -> m (BitMask n bit)
mergeBitMasksM m1 m2 = V.zipWithM mergeBitM m1 m2


mergeBitMasksErr :: ME.MonadError String m
                 => SemiMaskBit bit
                 => ShowableBit bit
                 => BitMask n bit
                 -> BitMask n bit
                 -> m (BitMask n bit)
mergeBitMasksErr mask1 mask2 =
  V.zipWithM mergeBitErr mask1 mask2
    `ME.catchError`
    (prependErr $ PP.render $ PP.text "mergeBitMasks: for masks:" PP.<+> prettyMask mask1 PP.<+> prettyMask mask2)


prettyMask :: forall bitmask bit n
            . ShowableBit bit
           => BitMask n bit
           -> PP.Doc
prettyMask mask = PP.hcat $ map (PP.text . showBit) (V.toList mask)


prettySegmentedMask :: forall bitmask bit n
                     . MaskBit bit
                    => ([bit] -> [bit])
                    -> BitMask n bit
                    -> PP.Doc
prettySegmentedMask endianness mask =
  PP.hcat $ PP.punctuate (PP.text ".") $ (map PP.hcat $ LS.chunksOf 8 (map go bits))
  where
    bits = endianness $ V.toList mask

    go :: bit -> PP.Doc
    go bit = PP.text (showBit bit)

data SomeBitMask bit where
  SomeBitMask :: forall n bit. 1 <= n => BitMask n bit -> SomeBitMask bit

deriving instance Functor SomeBitMask
deriving instance Show bit => Show (SomeBitMask bit)

instance Eq bit => Eq (SomeBitMask bit) where
  (SomeBitMask mask1) == (SomeBitMask mask2) = case testEquality (V.length mask1) (V.length mask2) of
    Just NR.Refl -> mask1 == mask2
    Nothing -> False

someBitMaskFromCons :: bit -> [bit] -> SomeBitMask bit
someBitMaskFromCons bit bits
  | Just (Some nr) <- NR.someNat (List.length bits)
  , NR.LeqProof <- NR.leqAdd (NR.leqRefl (NR.knownNat @1)) nr
  , Just mask <- V.fromList (NR.knownNat @1 `NR.addNat` nr) (bit : bits)
  = SomeBitMask mask

-- | A Represents a set of non-overlapping sub-masks of a given mask length.
-- Its canonical construction with 'mkBitSection' or 'mkBitSectionHiBit' yields only a single sub-mask,
-- however merging multiple 'BitSection's (through 'mergeBit') can result in multiple sub-masks being represented.
newtype BitSection n a = BitSection (BitMask n (WithBottom a))
  deriving (SemiMaskBit, Eq, Foldable, Functor)

instance (SemiMaskBit bit, KnownNat n, 1 <= n) => HasBottomMaskBit (BitSection n bit) where
  bottomBit = BitSection $ bottomBitMask NR.knownNat

instance MaskBit bit => Show (BitSection n bit) where
  show bitsect = showBitSection bitsect

instance PP.Pretty bit => PP.Pretty (BitSection n bit) where
  pPrint bitsect = prettyBitSection PP.pPrint bitsect

instance ShowableBit bit => ShowableBit (BitSection n bit) where
  showBit bs = showBitSection bs

-- | Low-level interface for constructing a 'BitSection' from a 'BitMask' of 'Maybe' bits.
maskAsBitSection :: BitMask n (WithBottom a) -> BitSection n a
maskAsBitSection mask = BitSection mask

-- | Construct a 'BitSection' out of a smaller sub-mask at a given position.
mkBitSection :: forall n posAt sectWidth a
              . (posAt + sectWidth <= n)
             => MaskBit a
             => NatRepr n
             -> NatRepr posAt
             -> BitMask sectWidth a
             -> BitSection n a
mkBitSection n posAt mask
  | sectWidth <- V.length mask
  , NR.Refl <- NR.plusComm posAt sectWidth
  , NR.LeqProof <- V.nonEmpty mask
  , prf1 :: NR.LeqProof (sectWidth + posAt) n <- NR.leqProof (sectWidth `NR.addNat` posAt ) n
  , prf2 :: NR.LeqProof sectWidth n <- NR.addIsLeqLeft1 prf1
  , prf3 :: NR.LeqProof 1 n <- NR.leqTrans (NR.leqProof (NR.knownNat @1) sectWidth) prf2
  = NR.withLeqProof prf3 $ BitSection $ V.replace posAt (fmap JustBit mask) (bottomBitMask n)


-- | Construct a 'BitSection' out of a smaller sub-mask at a given hibit position (i.e. considering
-- the first bit in the mask as index 'n' and the last as '0').
mkBitSectionHiBit :: forall n hiBit sectWidth a
                   . (hiBit + 1 <= n, sectWidth <= hiBit + 1)
                  => MaskBit a
                  => NatRepr n
                  -> NatRepr hiBit
                  -> BitMask sectWidth a
                  -> BitSection n a
mkBitSectionHiBit n hiBit mask
  | sectWidth <- V.length mask
  , hiBit_p1 <- hiBit `NR.addNat` NR.knownNat @1
  , posAt <- n `NR.subNat` hiBit_p1
  , prf1 :: NR.LeqProof (hiBit + 1) n <- NR.leqProof hiBit_p1 n
  , prf2 :: NR.LeqProof sectWidth (hiBit + 1) <- NR.leqProof sectWidth hiBit_p1
  , prf3 :: NR.LeqProof sectWidth n <- NR.leqTrans prf2 prf1
  , prf4 :: NR.LeqProof 1 sectWidth <- V.nonEmpty mask
  , prf5 :: NR.LeqProof 1 n <- NR.leqTrans prf4 prf3
  , prf6 :: NR.LeqProof n n <- NR.leqRefl n
  , prf7 :: NR.LeqProof sectWidth sectWidth <- NR.leqRefl sectWidth
  , prf8 :: NR.LeqProof (n - (hiBit + 1)) (n - sectWidth) <- NR.leqSub2 prf6 prf2
  , prf9 :: NR.LeqProof ((n - (hiBit + 1)) + sectWidth) ((n - sectWidth) + sectWidth) <- NR.leqAdd2 prf8 prf7
  , NR.Refl :: ((n - sectWidth) + sectWidth) :~: n <- NR.withLeqProof prf3 $ NR.minusPlusCancel n sectWidth
  , prf10 :: NR.LeqProof ((n - (hiBit + 1)) + sectWidth) n <- prf9
  = NR.withLeqProof prf10 $ mkBitSection n posAt mask

-- | Construct a 'BitSection' from a given list of bits at a given position. Returns 'Nothing'
-- if the sub-mask cannot fit into the given bitmask length.
bitSectionFromList :: forall bitmask n bit
                    . MaskBit bit
                   => Int
                   -> [bit]
                   -> NatRepr n
                   -> Maybe (BitSection n bit)
bitSectionFromList posInt bits nr
  | Just (Some bitLen) <- NR.someNat (List.length bits)
  , Just NR.LeqProof <- NR.testLeq (NR.knownNat @1) bitLen
  , Just (Some posRepr) <- NR.someNat posInt
  , Just mask <- V.fromList bitLen bits
  , Just NR.LeqProof <- NR.testLeq (posRepr `NR.addNat` (V.length mask)) nr
    = Just $ mkBitSection nr posRepr mask
  | otherwise = Nothing

-- | Construct a 'BitSection' from a given list of a bits at a given hibit position (i.e. considering
-- the first bit in the mask as index 'n' and the last as '0'). Returns 'Nothing'
-- if the sub-mask cannot fit into the given bitmask length.
bitSectionFromListHiBit :: forall bitmask n bit
                         . MaskBit bit
                        => Int
                        -> [bit]
                        -> NatRepr n
                        -> Maybe (BitSection n bit)
bitSectionFromListHiBit hiBitInt bits nr
  | Just (Some sectWidth) <- NR.someNat (List.length bits)
  , Just NR.LeqProof <- NR.testLeq (NR.knownNat @1) sectWidth
  , Just (Some hiBit) <- NR.someNat hiBitInt
  , Just mask <- V.fromList sectWidth bits
  , Just NR.LeqProof <- NR.testLeq (hiBit `NR.addNat` NR.knownNat @1) nr
  , Just NR.LeqProof <- NR.testLeq sectWidth (hiBit `NR.addNat` NR.knownNat @1)
    = Just $ mkBitSectionHiBit nr hiBit mask
  | otherwise = Nothing

-- | Compute a list of contiguous sub-masks represented by a 'BitSection' along with their position.
-- This is (morally) the reverse of 'bitSectionFromList', where
-- 'bitSectionFromList i bits nr == Just bitsect' iff
-- 'asContiguousSections bitsect == [(i, SomeBitMask mask)] && V.toList mask == bits'
asContiguousSections :: BitSection n a -> [(Int, SomeBitMask a)]
asContiguousSections (BitSection mask) =
  catMaybes $ map extract $ List.groupBy grouping $ map addIdx $ zip [0..] (V.toList mask)
  where
    extract :: [Maybe (Int, a)] -> Maybe (Int, SomeBitMask a)
    extract (Just (i, a) : rst) = Just (i, someBitMaskFromCons a (map (snd . fromJust) rst))
    extract _ = Nothing

    addIdx :: (Int, WithBottom a) -> Maybe (Int, a)
    addIdx (i, JustBit a) = Just (i, a)
    addIdx _ = Nothing

    grouping :: Maybe a -> Maybe a -> Bool
    grouping (Just _) (Just _) = True
    grouping Nothing Nothing = True
    grouping _ _ = False

sectTotalWidth :: BitSection n a -> Int
sectTotalWidth (BitSection mask) = V.lengthInt mask

sectTotalSetWidth :: BitSection n a -> Int
sectTotalSetWidth sect = sum $ map (\(_, SomeBitMask mask) -> V.lengthInt mask) $ asContiguousSections sect

-- | Print a 'BitSection' as a set of contiguous sub-masks, along with their hibit positions.
prettyBitSection :: (a -> PP.Doc) -> BitSection n a -> PP.Doc
prettyBitSection prettyBit bitsect =
  let
    chunks = map (\(bitPos, SomeBitMask mask) ->
                    (sectTotalWidth bitsect - bitPos - 1, map prettyBit $ V.toList mask)) $ asContiguousSections bitsect
  in PP.hcat $ map prettyBitSectionChunk chunks

prettyBitSectionChunk :: (Int, [PP.Doc]) -> PP.Doc
prettyBitSectionChunk (hiBit, bits) =
  (PP.hcat $ bits)
  PP.<> PP.text "<"
  PP.<> case List.length bits of
    1 -> PP.int hiBit
    x | x > 1 -> PP.int hiBit PP.<> PP.text ":" PP.<> PP.int (hiBit - x + 1)
    _ -> PP.empty
  PP.<> PP.text ">"

showBitSection :: ShowableBit a => BitSection n a -> String
showBitSection bitsect = PP.render $ prettyBitSection (PP.text . showBit) bitsect

-- | Merge the sub-masks of a 'BitSection' into the given 'BitMask' at their respective positions.
addSectionToMask :: ME.MonadError String m
                 => SemiMaskBit bit
                 => ShowableBit bit
                 => BitSection n bit
                 -> BitMask n bit
                 -> m (BitMask n bit)
addSectionToMask (BitSection mask) dest = do
  merged <- mergeBitMasksErr mask (fmap JustBit dest)
  -- The semantics of 'mergeBit' for 'WithBottom bit' guarantee that no 'BottomBit' bits are leftover after
  -- the merge.
  return $ fmap (\(JustBit bit) -> bit) merged

prependErr :: ME.MonadError String m => String -> String -> m a
prependErr msg err = ME.throwError $ msg ++ " " ++ err

-- | Flattens a 'BitSection' list into a single 'BitMask'. Overlapping sections are
-- merged according to 'mergeBit' addording to 'MaskBit', with merge failure (due to incompatible bits)
-- throwing an exception in the given error monad. Unset bits are left as 'bottomBit'.
computePattern :: forall bit m n
                 . (ME.MonadError String m, 1 <= n)
                => MaskBit bit
                => NatRepr n
                -> [BitSection n bit]
                -> m (BitMask n bit)
computePattern nr bitsects =
  go bitsects (bottomBitMask nr)
    `ME.catchError`
     (prependErr $ "computePattern: " ++ intercalate "," (map showBitSection bitsects))
  where
    go :: [BitSection n bit] -> BitMask n bit -> m (BitMask n bit)
    go [] mask = return $ mask
    go (bitsect : rst) mask = do
      resultMask <- addSectionToMask bitsect mask
        `ME.catchError`
        (prependErr $ "computePattern: for BitSection: " ++ showBitSection bitsect)
      go rst resultMask

-- | Derive a set of positive and negative masks from a given 'PropTree' of 'BitSection'.
-- e.g. turn ( x1x<4:2> && x10 <2:0> && !(010<4:2>) && !(11x<4:2>) into
--           ([x1x10], [ [010xx], [11xxx] ])
deriveMasks :: forall a m n
             . MaskBit a
            => ME.MonadError String m
            => 1 <= n
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

sectionOfConstraint :: 1 <= n => NatRepr n -> PropTree (BitSection n a) -> BitSection n ()
sectionOfConstraint nr constraints = NR.withKnownNat nr $ foldr go bottomBit constraints
  where
    go :: BitSection n a -> BitSection n () -> BitSection n ()
    go sect b = fromMaybe (error "impossible") $ fmap (const ()) sect `mergeBit` b

splitBitMask :: BitMask n (Either bit1 bit2) -> (BitMask n (WithBottom bit1), BitMask n (WithBottom bit2))
splitBitMask mask = (fmap doLeft mask, fmap doRight mask)
  where
    doLeft :: Either bit1 bit2 -> WithBottom bit1
    doLeft (Left bit1) = JustBit bit1
    doLeft _ = BottomBit

    doRight :: Either bit1 bit2 -> WithBottom bit2
    doRight (Right bit2) = JustBit bit2
    doRight _ = BottomBit

-- | A trie with 'BitMask' keys for a fixed mask length. BitMasks are matched according to
-- 'matchBit' of the 'bit' type: a lookup for a given mask will return all entries that would
-- match with the given one.
data MaskTrie bit n a where
  MaskLeaf :: a -> MaskTrie bit 0 a
  MaskNil :: forall bit n a. MaskTrie bit n a
  MaskNode :: forall bit n a. 1 <= n => (bit -> MaskTrie bit (n-1) a) -> MaskTrie bit n a

emptyMaskTrie :: MaskTrie bit n a
emptyMaskTrie = MaskNil

chooseLeaf :: forall n bit a. (SemiMaskBit bit, 1 <= n) => bit -> MaskTrie bit n a -> MaskTrie bit (n - 1) a
chooseLeaf bit tree = case tree of
  MaskNode node -> node bit
  MaskNil -> MaskNil

mapBranches :: forall n bit a c
             . 1 <= n
            => (bit -> MaskTrie bit (n - 1) a -> MaskTrie bit (n - 1) a)
            -> MaskTrie bit n a
            -> MaskTrie bit n a
mapBranches f tree = case tree of
  MaskNode node -> MaskNode (\bit -> f bit (node bit))
  MaskNil -> MaskNode (\bit -> f bit MaskNil)

updateMatching :: SemiMaskBit b => b -> (a -> a) -> b -> a -> a
updateMatching b f checkBit a = if checkBit `matchBit` b then f a else a

getMaskTreeLeaf :: MaskTrie bit 0 a -> Maybe a
getMaskTreeLeaf tree = case tree of
  MaskLeaf a -> Just a
  MaskNil -> Nothing

-- | Update the entries matching the given 'BitMask' in a 'MaskTrie'. This is an internal
-- interface as it can introduce inconsistencies in the trie.
updateMaskTrie :: forall n bit bitmask a
                . SemiMaskBit bit
               => BitMask n bit
               -> (Maybe a -> Maybe a)
               -> MaskTrie bit n a
               -> MaskTrie bit n a
updateMaskTrie mask f tree = case V.uncons mask of
  (b, Left NR.Refl) ->
    mapBranches (updateMatching b doUpdate) tree
  (b, Right mask') | NR.LeqProof <- V.nonEmpty mask ->
    mapBranches (updateMatching b (updateMaskTrie mask' f)) tree
  where
    doUpdate :: MaskTrie bit 0 a -> MaskTrie bit 0 a
    doUpdate leaf = case f $ getMaskTreeLeaf leaf of
      Just bit -> MaskLeaf bit
      Nothing -> MaskNil

-- | Add an element to a 'MaskTrie' with using a given 'BitMask' as its key.
addToMaskTrie :: forall n bitmask bit a
               . SemiMaskBit bit
              => Semigroup a
              => BitMask n bit
              -> a
              -> MaskTrie bit n a
              -> MaskTrie bit n a
addToMaskTrie mask a tree = updateMaskTrie mask go tree
  where
    go :: Maybe a -> Maybe a
    go Nothing = Just a
    go (Just a') = Just $ a <> a'

-- | Retrieve the entries from the trie with keys that match (according to 'matchBit') the given mask.
lookupMaskTrie :: forall a n bitmask bit m
                . SemiMaskBit bit
               => Monoid a
               => BitMask n bit
               -> MaskTrie bit n a
               -> a
lookupMaskTrie mask tree = case V.uncons mask of
  (b, Left NR.Refl) ->
    fromMaybe mempty $ getMaskTreeLeaf (chooseLeaf b tree)
  (b, Right mask') | NR.LeqProof <- V.nonEmpty mask ->
    lookupMaskTrie mask' (chooseLeaf b tree)
