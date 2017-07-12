{-# LANGUAGE PolyKinds #-}
module Data.EnumF ( EnumF(..), enumCompareF ) where

import           Data.Parameterized.Classes ( OrderingF(..) )
import qualified Data.Set.NonEmpty as NES
import           Unsafe.Coerce ( unsafeCoerce )

-- | Enumerable parameterized types
class EnumF a where
  -- | Map each element of a parameterized type to a unique 'Int'
  enumF :: a p -> Int
  -- | For an element of a parameterized type, return all of the
  -- elements with the same parameter. An element is always congruent
  -- to itself, so the set is non-empty.
  congruentF :: a p -> NES.Set (a p)

enumCompareF :: (EnumF a) => a p1 -> a p2 -> OrderingF p1 p2
enumCompareF x y =
  case compare (enumF x) (enumF y) of
    LT -> LTF
    EQ -> unsafeCoerce EQF
    GT -> GTF
