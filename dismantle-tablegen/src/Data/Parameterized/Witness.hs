{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Parameterized.Witness (
  Witness(..)
  ) where

import GHC.Exts ( Constraint )

import Data.Proxy ( Proxy(..) )

import qualified Data.Parameterized.Classes as P

data Witness (c :: k -> Constraint) (f :: k -> *) (x :: k) where
  Witness :: (c x) => f x -> Witness c f x

instance (P.TestEquality f) => P.TestEquality (Witness c f) where
  testEquality (Witness x) (Witness y) = (\P.Refl -> P.Refl) <$> P.testEquality x y

instance (P.OrdF f) => P.OrdF (Witness c f) where
  compareF (Witness x) (Witness y) =
    case P.compareF x y of
      P.LTF -> P.LTF
      P.EQF -> P.EQF
      P.GTF -> P.GTF

instance (Show (f x)) => Show (Witness c f x) where
  show (Witness d) = "Witness (" ++ show d ++ ")"

witnessWithShow :: forall p c f q tp a. (P.ShowF f) => p (Witness c f) -> q tp -> (Show (Witness c f tp) => a) -> a
witnessWithShow _ _ = P.withShow (Proxy @f) (Proxy @tp)

instance (P.ShowF f) => P.ShowF (Witness c f) where
  withShow = witnessWithShow
