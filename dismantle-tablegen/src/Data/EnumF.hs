{-# LANGUAGE PolyKinds #-}
module Data.EnumF ( EnumF(..), enumCompareF ) where

import           Data.Parameterized.Classes ( OrderingF(..), OrdF(..) )
import qualified Data.Set as S
import           Unsafe.Coerce ( unsafeCoerce )

-- | Enumerable parameterized types
class EnumF a where
  -- | Map each element of a parameterized type to a unique 'Int'
  enumF :: a p -> Int
  -- | For an element of a parameterized type, return all of the elements with
  -- the same parameter
  congruentF :: a p -> S.Set (a p)

enumCompareF :: (EnumF a) => a p1 -> a p2 -> OrderingF p1 p2
enumCompareF x y =
  case compare (enumF x) (enumF y) of
    LT -> LTF
    EQ -> unsafeCoerce EQF
    GT -> GTF
