{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Data.Parameterized.ShapedList
  ( -- * ShapedList
    ShapedList(..)
  , lengthFC
    -- * Indexing
  , Index(..)
  , indexShapedList
  , updateShapedList
  , indexAsInt
  , fmapFCIndexed
  , foldrFCIndexed
  , traverseFCIndexed
    -- * Type synonyms
  , ShapeRepr
  ) where

import Data.Parameterized.Classes
import Data.Parameterized.SymbolRepr ( SymbolRepr )
import Data.Parameterized.TraversableFC

--------------------------------------------------------------------------------
-- ShapedList

-- | An implementation of heterogenous lists, mapping some functor over a
-- type-level list.
data ShapedList :: (k -> *) -> [k] -> * where
  Nil  :: ShapedList f '[]
  (:>) :: f tp -> ShapedList f tps -> ShapedList f (tp ': tps)

infixr 5 :>

instance (ShowF f) => Show (ShapedList f sh) where
  show Nil = "Nil"
  show (elt :> rest) = showF elt ++ " :> " ++ show rest

instance (ShowF f) => ShowF (ShapedList f)

instance (TestEquality f) => TestEquality (ShapedList f) where
  testEquality Nil Nil = Just Refl
  testEquality (i1 :> rest1) (i2 :> rest2) = do
    Refl <- testEquality i1 i2
    Refl <- testEquality rest1 rest2
    return Refl
  testEquality _ _ = Nothing

instance (OrdF f) => OrdF (ShapedList f) where
  compareF Nil Nil = EQF
  compareF Nil (_ :> _) = LTF
  compareF (_ :> _) Nil = GTF
  compareF (i1 :> rest1) (i2 :> rest2) =
    case compareF i1 i2 of
      LTF -> LTF
      EQF -> case compareF rest1 rest2 of
               LTF -> LTF
               EQF -> EQF
               GTF -> GTF
      GTF -> GTF

instance FunctorFC ShapedList where
  fmapFC _ Nil = Nil
  fmapFC f (x :> xs) = f x :> fmapFC f xs

instance FoldableFC ShapedList where
  foldrFC _ z Nil = z
  foldrFC f z (x :> xs) = f x (foldrFC f z xs)

instance TraversableFC ShapedList where
  traverseFC _ Nil = pure Nil
  traverseFC f (x :> xs) = (:>) <$> f x <*> traverseFC f xs

instance KnownRepr (ShapedList f) '[] where
  knownRepr = Nil

instance (KnownRepr f s, KnownRepr (ShapedList f) sh) => KnownRepr (ShapedList f) (s ': sh) where
  knownRepr = knownRepr :> knownRepr

lengthFC :: (FoldableFC t) => t e c -> Int
lengthFC = foldrFC (const (+1)) 0

--------------------------------------------------------------------------------
-- Indexed operations

-- | Represents an index into a type-level list. Used in place of integers to
--   1. ensure that the given index *does* exist in the list
--   2. guarantee that it has the given kind
data Index :: [k] -> k -> * where
  IndexHere :: Index (x ': sh) x
  IndexThere :: Index sh x -> Index (x' ': sh) x
deriving instance Eq (Index sh x)
deriving instance Show (Index sh x)

instance ShowF (Index sh)

instance TestEquality (Index sh) where
  IndexHere `testEquality` IndexHere = Just Refl
  IndexThere idx1 `testEquality` IndexThere idx2 = testEquality idx1 idx2
  _ `testEquality` _ = Nothing

instance OrdF (Index sh) where
  IndexHere `compareF` IndexHere = EQF
  IndexHere `compareF` IndexThere _ = LTF
  IndexThere _ `compareF` IndexHere = GTF
  IndexThere idx1 `compareF` IndexThere idx2 =
    case idx1 `compareF` idx2 of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF

instance Ord (Index sh x) where
  x `compare` y = toOrdering $ x `compareF` y

-- | Evaluate an index for a given operand list.
indexShapedList :: ShapedList f sh -> Index sh s -> f s
-- Why not destructure @vals@ in the argument position? GHC gives a warning
-- about not handling the Nil case of vals. This way, GHC verifies that the
-- pattern-matching is exhaustive.
indexShapedList vals IndexHere = case vals of x :> _ -> x
indexShapedList vals (IndexThere th) = case vals of _ :> rest -> indexShapedList rest th

-- | Update the 'ShapedList' at an index
updateShapedList :: ShapedList f sh -> Index sh s -> (f s -> f s) -> ShapedList f sh
updateShapedList vals IndexHere upd =
  case vals of
    x :> rest -> upd x :> rest
updateShapedList vals (IndexThere th) upd =
  case vals of
    x :> rest -> x :> updateShapedList rest th upd

indexAsInt :: Index sh tp -> Int
indexAsInt ix =
  case ix of
    IndexHere -> 0
    IndexThere ix' -> 1 + indexAsInt ix'

fmapFCIndexed :: forall f g sh
               . (forall tp . Index sh tp -> f tp -> g tp)
              -> ShapedList f sh
              -> ShapedList g sh
fmapFCIndexed f = go id
  where
    go :: forall sh'
        . (forall tp . Index sh' tp -> Index sh tp)
       -> ShapedList f sh'
       -> ShapedList g sh'
    go g l =
      case l of
        Nil -> Nil
        e :> rest -> f (g IndexHere) e :> go (\ix -> g (IndexThere ix)) rest

foldrFCIndexed :: forall sh a b . (forall tp . Index sh tp -> a tp -> b -> b) -> b -> ShapedList a sh -> b
foldrFCIndexed f seed0 l = go id l seed0
  where
    go :: forall tps
        . (forall tp . Index tps tp -> Index sh tp)
       -> ShapedList a tps
       -> b
       -> b
    go g ops b =
      case ops of
        Nil -> b
        a :> rest -> f (g IndexHere) a (go (\ix -> g (IndexThere ix)) rest b)

traverseFCIndexed :: forall a b sh t
                   . (Applicative t)
                  => (forall tp . Index sh tp -> a tp -> t (b tp))
                  -> ShapedList a sh
                  -> t (ShapedList b sh)
traverseFCIndexed f = go id
  where
    go :: forall tps . (forall tp . Index tps tp -> Index sh tp)
       -> ShapedList a tps
       -> t (ShapedList b tps)
    go g l =
      case l of
        Nil -> pure Nil
        e :> rest -> (:>) <$> f (g IndexHere) e <*> go (\ix -> g (IndexThere ix)) rest

--------------------------------------------------------------------------------
-- Type synonyms

-- | A runtime representation of a type-level list of symbols.
type ShapeRepr = ShapedList SymbolRepr
