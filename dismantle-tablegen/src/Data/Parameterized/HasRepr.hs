{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
module Data.Parameterized.HasRepr
  ( HasRepr(..)
  ) where

import Data.Kind ( Type )

-- | A multi-parameter type class that allows one to represent that a
-- parameterized type value has some representative type such as a TypeRepr.
class HasRepr (f :: k -> Type) (v :: k -> Type) | f -> v where
  typeRepr :: f tp -> v tp
