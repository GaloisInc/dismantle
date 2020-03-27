{-|
Module           : Data.PropTree
Copyright        : (c) Galois, Inc 2019-2020
Maintainer       : Daniel Matichuk <dmatichuk@galois.com>

A container for trees of proposition-like elements.

-}

{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.PropTree
  ( -- * Constructing PropTrees
    PropTree
  , collapse
  , clause
  , negate
  , partition
  
  -- * Destructing PropTrees
  , flatten
  , splitClauses
  , negatedSubtrees
  , toConjunctsAndDisjuncts
  
  -- * Pretty-printing
  , prettyPropTree
  
  ) where

import Prelude hiding (negate)

import qualified Text.PrettyPrint.HughesPJClass as PP

-- | Representation of a propositional formula. Trees are built as atoms (via 'clause'), by
-- concatenating trees together (via 'mconcat'), or by negating a tree (via 'negate').
data PropTree a =
    PropLeaf a
  | PropNegate (PropTree a)
  | PropList [PropTree a]
  deriving (Functor, Foldable, Traversable, Show, Eq)

-- | Create an singleton 'PropTree' with a given element.
clause :: a -> PropTree a
clause a = PropLeaf a

-- | Collapse a 'PropTree' over lists by considering each list as a conjunction of elements.
collapse :: PropTree [a] -> PropTree a
collapse tree = case tree of
  PropLeaf as -> mconcat (map clause as)
  PropNegate tree' -> negate (collapse tree')
  PropList trees -> mconcat $ map collapse trees

-- | Concat 'PropTrees' (representing logical conjunction) while avoiding redundant nodes
concatTrees :: [PropTree a] -> PropTree a
concatTrees [tree] = tree
concatTrees trees = PropList (filter (not . null) trees)

-- | Negation of a 'PropTree' while avoiding double-negation
negate :: PropTree a -> PropTree a
negate (PropList []) = PropList []
negate (PropNegate a) = a
negate a = PropNegate a

instance Semigroup (PropTree a) where
  a <> b = concatTrees [a, b]

instance Monoid (PropTree a) where
  mempty = PropList []
  mconcat = concatTrees

-- | Partition a 'PropTree' over an 'Either' into two trees.
partition :: PropTree (Either a b) -> (PropTree a, PropTree b)
partition tree = case tree of
  PropLeaf e -> case e of
    Left a -> (clause a, mempty)
    Right b -> (mempty, clause b)
  PropList es ->
    let (as, bs) = unzip $ map partition es
    in (mconcat as, mconcat bs)
  PropNegate p ->
    let (a, b) = partition p
    in (negate a, negate b)

-- | Flatten a 'PropTree' into a list of positive clauses.
-- Returns 'Nothing' if the tree contains negations.
flatten :: PropTree a -> Maybe [a]
flatten tree = case tree of
  PropLeaf a -> return $ [a]
  PropList as -> concat <$> mapM flatten as
  PropNegate _ -> Nothing

-- | All subtrees in the given 'PropTree' which are under a single negation.
negatedSubtrees :: PropTree a -> [PropTree a]
negatedSubtrees tree = case tree of
  PropLeaf _ -> []
  PropList as -> concat $ map negatedSubtrees as
  PropNegate p -> [p]

-- | Separate a 'PropTree' into a list of positive clauses, and a list of 'PropTree's
-- which are under a single negation in the original tree.
splitClauses :: PropTree a -> ([a], [PropTree a])
splitClauses tree =
  let
    (positive, negative) = partition (go tree)
  in case flatten positive of
      Just as -> (as, negatedSubtrees negative)
      Nothing -> error "Unreachable"
  where
    go :: PropTree a -> PropTree (Either a a)
    go (PropLeaf a) = clause $ Left a
    go (PropList as) = mconcat $ map go as
    go (PropNegate p) = negate $ fmap Right p


-- | Split a 'PropTree' into a list (conjunction) of positive clauses, and
-- a list (conjunction) of lists (disjunction) of negated clauses.
-- e.g.
--
-- @
-- (A & B) & !(A & C) & !(C & D) ==
-- (A & B) & (!A | !C) & (!C | !D) ==>
-- ([A,B], [[A, C], [C, D]])
-- @
--
-- Returns 'Nothing' if a tree contains double-negation (e.g. @A & !(A & !C)@)
toConjunctsAndDisjuncts :: PropTree a -> Maybe ([a], [[a]])
toConjunctsAndDisjuncts tree = do
  let (positive, negativeTrees) = splitClauses tree
  negativess <- mapM flatten negativeTrees
  return (positive, negativess)

-- | Print a 'PropTree', using the given function for elements.
prettyPropTree :: forall a. (a -> PP.Doc) -> PropTree a -> PP.Doc
prettyPropTree f tree = go tree
  where
    go :: PropTree a -> PP.Doc
    go (PropLeaf a) = f a
    go (PropList as) = PP.parens (PP.hcat (PP.punctuate (PP.text " && ") (map go as)))
    go (PropNegate p) = PP.text "!" PP.<> (go p)

instance PP.Pretty a => PP.Pretty (PropTree a) where
  pPrint tree = prettyPropTree PP.pPrint tree
