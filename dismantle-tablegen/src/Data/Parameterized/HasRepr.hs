{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
module Data.Parameterized.HasRepr
  ( HasRepr(..)
  ) where

-- | A multi-parameter type class that allows one to represent that a
-- parameterized type value has some representative type such as a TypeRepr.
class HasRepr (f :: k -> *) (v :: k -> *) | f -> v where
  typeRepr :: f tp -> v tp
