{-# LANGUAGE PolyKinds #-}
module Data.EnumF ( EnumF(..) ) where

import qualified Data.Set as S

-- | Enumerable parameterized types
class EnumF a where
  -- | Map each element of a parameterized type to a unique 'Int'
  enumF :: a p -> Int
  -- | For an element of a parameterized type, return all of the elements with
  -- the same parameter
  congruentF :: a p -> S.Set (a p)
