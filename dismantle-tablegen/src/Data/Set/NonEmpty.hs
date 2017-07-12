module Data.Set.NonEmpty (
  Set,
  singleton,
  fromList,
  flatten,
  view
  ) where

import qualified Data.Semigroup as SG
import qualified Data.Set as S

data Set a = NonEmptySet a (S.Set a)
  deriving (Show)

singleton :: (Ord a) => a -> Set a
singleton a = fromList a []

fromList :: (Ord a) => a -> [a] -> Set a
fromList a as = NonEmptySet a (S.fromList as)

flatten :: (Ord a) => Set a -> S.Set a
flatten (NonEmptySet a s) = S.insert a s

-- | View a non-empty set as an element and a possibly empty set.
view :: Set a -> (a, S.Set a)
view (NonEmptySet a as) = (a, as)

instance (Ord a) => SG.Semigroup (Set a) where
  NonEmptySet a1 as1 <> NonEmptySet a2 as2 =
    NonEmptySet a1 (S.insert a2 (S.union as1 as2))

instance Foldable Set where
  foldr op acc (NonEmptySet a as) = foldr op (a `op` acc) as
