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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | An abstraction of 'ByteTrie' bits that allows abstract types to be considered "bit-like", with
-- corresponding container datatypes for constructing masks out of them.
module Data.BitMask
  ( IsMaskBit(..)
  , AsBit(..)
  , equivBitMask
  , matchingBitMask
  , BitSection(..)
  , mkBitSection
  , bitSectionFromList
  , QuasiBit(..)
  , BitMask
  , computePattern
  , readBit
  , readQuasiBit
  , deriveMasks
  , prettyMask
  , prettySegmentedMask
  , V.toList
  , V.fromList
  , MaskTrie
  , emptyMaskTree
  , addMaskToTree
  , updateMaskInTree
  , lookupMaskFromTree
  , matchMaskFromTree
  )
  where

import           Prelude hiding ( zipWith, length )

import           GHC.TypeNats
import           Control.Monad.Identity ( runIdentity )
import qualified Control.Monad.Except as ME
import           Control.Monad ( unless )
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

class IsMaskBit a where
  defaultBit :: a

  showBit :: a -> String
  mergeBit :: a -> a -> Maybe a
  equivBit :: a -> a -> Bool

  matchingBit :: a -> a -> Bool
  matchingBit a1 a2 = isJust (mergeBit a1 a2)

mergeBitM :: ME.MonadError (a, a) m => IsMaskBit a => a -> a -> m a
mergeBitM a1 a2 = case mergeBit a1 a2 of
  Just a -> return a
  Nothing -> ME.throwError (a1, a2)

mergeBitErr :: ME.MonadError String m => IsMaskBit bit => bit -> bit -> m bit
mergeBitErr m1 m2 = case mergeBitM m1 m2 of
  Left (a1, a2) -> ME.throwError $ "incompatible bits: " ++ showBit a1 ++ " " ++ showBit a2
  Right a -> return a

instance IsMaskBit BT.Bit where
  defaultBit = BT.Any

  showBit a = case a of
    BitSet -> "1"
    BitUnset -> "0"
    BitAny -> "x"

  equivBit a1 a2 = a1 == a2

  mergeBit a1 a2 = case (asBit a1, asBit a2) of
     (BT.ExpectedBit x, BT.ExpectedBit y) | x == y -> return a1
     (BT.Any, _) -> return a2
     (_, BT.Any) -> return a1
     _ -> fail "Incompatible bits"

instance IsMaskBit Bool where
  defaultBit = False

  showBit b = showBit (asBit b)
  mergeBit b1 b2 = if b1 == b2 then Just b1 else Nothing
  equivBit b1 b2 = b1 == b2
  matchingBit b1 b2 = b1 == b2


instance AsBit Bool where
  asBit True = BT.ExpectedBit True
  asBit False = BT.ExpectedBit False

class AsBit a where
  asBit :: a -> BT.Bit

instance AsBit BT.Bit where
  asBit = id

readBit :: String -> Maybe BT.Bit
readBit s = case s of
  "1" -> Just $ BitSet
  "0" -> Just $ BitUnset
  "x" -> Just $ BitAny
  "" -> Just $ BitAny
  _ -> Nothing

instance IsMaskBit a => IsMaskBit (Maybe a) where
  defaultBit = Nothing
  showBit mbit = case mbit of
    Nothing -> "?"
    Just bit -> showBit bit

  equivBit mb1 mb2 = case (mb1, mb2) of
    (Just b1, Just b2) -> equivBit b1 b2
    (Nothing, Nothing) -> True
    _ -> False

  mergeBit mb1 mb2 = case (mb1, mb2) of
    (Just b1, Just b2) -> Just $ mergeBit b1 b2
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
  showBit qb = case qb of
    Bit b -> showBit b
    QBit True -> "I"
    QBit False -> "O"

  defaultBit = Bit BT.Any

  equivBit qb1 qb2 = equivBit (asBit qb1) (asBit qb2)

  mergeBit qb1 qb2 = case (qb1, qb2) of
    (QBit x, QBit y) | x == y -> return $ QBit y
    (QBit x, Bit (BT.ExpectedBit y)) | x == y -> return $ Bit $ BT.ExpectedBit y
    (Bit (BT.ExpectedBit y), QBit x) | x == y -> return $ Bit $ BT.ExpectedBit y
    (_, Bit BT.Any) -> return qb1
    (Bit BT.Any, _) -> return qb2
    (Bit b1, Bit b2) -> Bit <$> mergeBit b1 b2
    _ -> fail "Incompatible qbits"

instance AsBit QuasiBit where
  asBit qb = case qb of
    Bit b -> b
    QBit _ -> BitAny

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

type BitMask n bit = V.Vector n bit

instance (IsMaskBit bit, 1 <= n, KnownNat n) => IsMaskBit (BitMask n bit) where
  defaultBit = defaultBitMask NR.knownNat
  showBit m = PP.render (prettyMask m)
  mergeBit m1 m2 = V.zipWithM mergeBit m1 m2
  equivBit = equivBitMask
  matchingBit = matchingBitMask

defaultBitMask :: forall bitmask n bit. 1 <= n => IsMaskBit bit => NR.NatRepr n -> BitMask n bit
defaultBitMask nr
  | NR.Refl <- NR.minusPlusCancel nr (NR.knownNat @1)
  = V.generate (NR.decNat nr) (\_ -> defaultBit)

mergeBitMasksM :: ME.MonadError (bit, bit) m => IsMaskBit bit => BitMask n bit -> BitMask n bit -> m (BitMask n bit)
mergeBitMasksM m1 m2 = V.zipWithM mergeBitM m1 m2

equivBitMask :: IsMaskBit bit => BitMask n bit -> BitMask n bit -> Bool
equivBitMask m1 m2 = all id (V.zipWith equivBit m1 m2)

matchingBitMask :: IsMaskBit bit => BitMask n bit -> BitMask n bit -> Bool
matchingBitMask m1 m2 = all id (V.zipWith matchingBit m1 m2)


mergeBitMasksErr :: ME.MonadError String m
              => IsMaskBit bit
              => BitMask n bit
              -> BitMask n bit
              -> m (BitMask n bit)
mergeBitMasksErr mask1 mask2 =
  V.zipWithM mergeBitErr mask1 mask2
    `ME.catchError`
    (prependErr $ PP.render $ PP.text "mergeBitMasks: for masks:" PP.<+> prettyMask mask1 PP.<+> prettyMask mask2)


prettyMask :: forall bitmask bit n
            . IsMaskBit bit
           => BitMask n bit
           -> PP.Doc
prettyMask mask = PP.hcat $ map (PP.text . showBit) (V.toList mask)


prettySegmentedMask :: forall bitmask bit n
                     . IsMaskBit bit
                    => ([bit] -> [bit])
                    -> BitMask n bit
                    -> PP.Doc
prettySegmentedMask endianness mask =
  PP.hcat $ PP.punctuate (PP.text ".") $ (map PP.hcat $ LS.chunksOf 8 (map go bits))
  where
    bits = endianness $ V.toList mask

    go :: bit -> PP.Doc
    go bit = PP.text (showBit bit)


-- | A wrapper around a 'BitMask' representing a slice of bits.
data BitSection n a where
  BitSection :: BitMask n (Maybe a) -> BitSection n a


deriving instance Functor (BitSection n)
deriving instance Foldable (BitSection n)
deriving instance Eq a => Eq (BitSection n a)


instance (IsMaskBit bit) => Show (BitSection n bit) where
  show bitsect = showBitSection bitsect

instance (PP.Pretty bit) => PP.Pretty (BitSection n bit) where
  pPrint bitsect = prettyBitSection PP.pPrint bitsect

instance (IsMaskBit bit, KnownNat n, 1 <= n) => IsMaskBit (BitSection n bit) where
  defaultBit = BitSection $ defaultBitMask NR.knownNat
  showBit bs = showBitSection bs
  mergeBit (BitSection bs1) (BitSection bs2) = BitSection <$> mergeBit bs1 bs2
  equivBit (BitSection m1) (BitSection m2) = all id (V.zipWith equivBit m1 m2)

-- | Construct a 'BitSection' out of a smaller mask at a given position.
mkBitSection :: forall n posAt sectWidth a
              . (posAt + sectWidth <= n)
             => IsMaskBit a
             => NatRepr n
             -> NatRepr posAt
             -> BitMask sectWidth a
             -> BitSection n a
mkBitSection n posAt mask
  | sectWidth <- V.length mask
  , NR.Refl <- NR.plusComm posAt sectWidth
  , NR.LeqProof <- V.nonEmpty mask
  , (prf1 :: NR.LeqProof (sectWidth + posAt) n) <- NR.leqProof (sectWidth `NR.addNat` posAt ) n
  , (prf2 :: NR.LeqProof sectWidth n) <- NR.addIsLeqLeft1 prf1
  , (prf3 :: NR.LeqProof 1 n) <- NR.leqTrans (NR.leqProof (NR.knownNat @1) sectWidth) prf2
  = NR.withLeqProof prf3 $ BitSection $ V.replace posAt (fmap Just mask) (defaultBitMask n)

bitSectionFromList :: forall bitmask n bit
              . IsMaskBit bit
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

-- | Recover traditional bitsection form
asContiguousSections :: BitSection n a -> [(Int, [a])]
asContiguousSections (BitSection mask) =
  catMaybes $ map extract $ List.groupBy grouping $ map addIdx $ zip [0..] (V.toList mask)
  where
    extract :: [Maybe (Int, a)] -> Maybe (Int, [a])
    extract (Just (i, a) : rst) = Just (i, a : map (snd . fromJust) rst)
    extract _ = Nothing

    addIdx :: (Int, Maybe a) -> Maybe (Int, a)
    addIdx (i, Just a) = Just (i, a)
    addIdx _ = Nothing

    grouping :: Maybe a -> Maybe a -> Bool
    grouping (Just _) (Just _) = True
    grouping Nothing Nothing = True
    grouping _ _ = False

sectWidth :: BitSection n a -> Int
sectWidth (BitSection mask) = V.lengthInt mask

prettyBitSection :: (a -> PP.Doc) -> BitSection n a -> PP.Doc
prettyBitSection prettyBit bitsect =
  let
    chunks = asContiguousSections bitsect
    pretty = map (\(bitPos, bits) -> prettyBitSectionChunk (sectWidth bitsect - bitPos, map prettyBit bits)) chunks
  in PP.hcat pretty

prettyBitSectionChunk :: (Int, [PP.Doc]) -> PP.Doc
prettyBitSectionChunk (hiBit, bits) =
  (PP.hcat $ bits)
  PP.<> PP.text "<"
  PP.<> case List.length bits of
    1 -> PP.int hiBit
    x | x > 1 -> PP.int hiBit PP.<> PP.text ":" PP.<> PP.int (hiBit - x + 1)
    _ -> PP.empty
  PP.<> PP.text ">"

showBitSection :: IsMaskBit a => BitSection n a -> String
showBitSection bitsect = PP.render $ prettyBitSection (PP.text . showBit) bitsect


addSectionToMask :: ME.MonadError String m
                 => IsMaskBit bit
                 => BitSection n bit
                 -> BitMask n bit
                 -> m (BitMask n bit)
addSectionToMask (BitSection mask) dest = do
  merged <- mergeBitMasksErr mask (fmap Just dest)
  return $ fmap (fromMaybe defaultBit) merged

prependErr :: ME.MonadError String m => String -> String -> m a
prependErr msg err = ME.throwError $ msg ++ " " ++ err

-- | Flattens a 'BitSection' list into a single 'BitMask'. Overlapping sections are
-- merged according to 'mergeBits' from 'IsMaskBit', with merge failure (due to incompatible bits)
-- throwing an exception in the given error monad. Unset bits are left as the default bit value.
computePattern :: forall bit m n
                 . (ME.MonadError String m, 1 <= n)
                => IsMaskBit bit
                => NatRepr n
                -> [BitSection n bit]
                -> m (BitMask n bit)
computePattern nr bitsects =
  go bitsects (defaultBitMask nr)
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
-- e.g. turn ( x1x<0:2> && 11 <2:4> && !(010<0:2>) && !(11x<0:2>) into
--           ([x1x11], [ [010xx], [11xxx] ])
deriveMasks :: forall a m n
             . IsMaskBit a
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


-- | A specialization of a trie with fixed-length lookups and indexing with any
-- appropriately-sized bitmask.
data MaskTrie bit n a where
  MaskLeaf :: a -> MaskTrie bit 0 a
  MaskNil :: forall bit n a. MaskTrie bit n a
  MaskNode :: forall bit n a. 1 <= n => (bit -> MaskTrie bit (n-1) a) -> MaskTrie bit n a


emptyMaskTree :: MaskTrie bit n a
emptyMaskTree = MaskNil

chooseLeaf :: forall n bit a. (IsMaskBit bit, 1 <= n) => bit -> MaskTrie bit n a -> MaskTrie bit (n - 1) a
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

updateMatching :: IsMaskBit b => b -> (a -> a) -> b -> a -> a
updateMatching b f checkbit a = if b `matchingBit` checkbit then f a else a

getMaskTreeLeaf :: MaskTrie bit 0 a -> Maybe a
getMaskTreeLeaf tree = case tree of
  MaskLeaf a -> Just a
  MaskNil -> Nothing

updateMaskInTree :: forall n bit bitmask a
                  . IsMaskBit bit
                 => BitMask n bit
                 -> (Maybe a -> a)
                 -> MaskTrie bit n a
                 -> MaskTrie bit n a
updateMaskInTree mask f tree = case V.uncons mask of
  (b, Left NR.Refl) ->
    mapBranches (updateMatching b (\leaf -> MaskLeaf (f $ getMaskTreeLeaf leaf))) tree
  (b, Right mask') | NR.LeqProof <- V.nonEmpty mask ->
    mapBranches (updateMatching b (updateMaskInTree mask' f)) tree

addMaskToTree :: forall n bitmask bit a
               . IsMaskBit bit
              => BitMask n bit
              -> a
              -> MaskTrie bit n [a]
              -> MaskTrie bit n [a]
addMaskToTree mask a tree = updateMaskInTree mask go tree
  where
    go :: Maybe [a] -> [a]
    go Nothing = [a]
    go (Just as) = a : as

lookupMaskFromTree :: forall a n bitmask bit m
                    . IsMaskBit bit
                   => BitMask n bit
                   -> MaskTrie bit n a
                   -> Maybe a
lookupMaskFromTree mask tree = case V.uncons mask of
  (b, Left NR.Refl) ->
    getMaskTreeLeaf (chooseLeaf b tree)
  (b, Right mask') | NR.LeqProof <- V.nonEmpty mask ->
    lookupMaskFromTree mask' (chooseLeaf b tree)

matchMaskFromTree :: forall a n b m
                   . IsMaskBit b
                  => BitMask n b
                  -> [BitMask n b]
                  -> MaskTrie b n [(a, BitMask n b)]
                  -> [a]
matchMaskFromTree mask negmasks tree = fromMaybe [] $ fmap (catMaybes . map go) $ lookupMaskFromTree mask tree
  where
    go :: (a, BitMask n b) -> Maybe a
    go (a, mask') = if any (\negmask -> mask' `matchingBitMask` negmask) negmasks then Nothing else Just a
