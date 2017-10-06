{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Parameterized.Lift (
  LiftF(..)
  ) where

import Data.Proxy ( Proxy(..) )
import Language.Haskell.TH ( Exp, Q )
import Language.Haskell.TH.Syntax ( Lift(..) )

class LiftF (f :: k -> *) where
  withLift :: p f -> q tp -> (Lift (f tp) => a) -> a

  default withLift :: (Lift  (f tp)) => p f -> q tp -> (Lift (f tp) => a) -> a
  withLift _ _ x = x

  liftF :: forall tp . f tp -> Q Exp
  liftF x = withLift (Proxy @f) (Proxy @tp) (lift x)

