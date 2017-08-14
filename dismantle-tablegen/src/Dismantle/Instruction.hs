{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Dismantle.Instruction (
  -- OperandList(..),
  GenericInstruction(..),
  Annotated(..),
  OpcodeConstraints,
  -- Index(..),
  -- mapOpcode,
  traverseOpcode,
  -- operandListLength,
  -- mapOperandList,
  -- mapOperandListIndexed,
  -- traverseOperandList,
  -- traverseOperandListIndexed,
  -- foldrOperandList,
  -- foldrMOperandList,
  -- indexOpList,
  -- indexAsInt
  ) where

import qualified Data.Type.Equality as E
import Data.Typeable ( Typeable )

import Data.EnumF ( EnumF(..) )
import Data.Parameterized.Classes ( ShowF(..), OrdF(..) )
import Data.Parameterized.Some ( Some(..) )
import Data.Parameterized.ShapedList ( ShapedList )

-- | A wrapper to allow operands to be easily annotated with arbitrary
-- data (of kind '*' for now).
--
-- Assuming a definition of an instruction like the following
--
-- > type MyInstruction = Instruction MyISA OperandType
--
-- Usage of 'Annotated' would be something like:
--
-- > type MyAnnotatedInstruction = Instruction MyISA (Annotated OperandType AnnotationType)
--
-- The conversion to this type could be accomplished with
-- 'mapOperandList' of the 'Annotated' constructor.  The annotation is
-- first so that a partial application during 'mapOperandList' is
-- simplified.
data Annotated a o tp = Annotated a (o tp)

-- | The type of instructions
--
-- This type is has two type parameters:
--
-- 1) The *tag* type, which is an enumeration of all of the possible
-- instructions for the architecture, with each constructor
-- parameterized by its *shape*.  The shape is the list of arguments
-- the instruction takes represented at the type level.
--
-- 2) The *operand* type, which represents all of the possible types
-- of operand in the ISA.  For example, reg32, immediate32,
-- immediate16, etc.
--
-- This type actually requires *three* auxiliary data types: the tag
-- type, the operand type, and a separate data type to act as
-- type-level tags on operands.
--
-- The name is 'GenericInstruction' so that specific aliases can be
-- instantiated as just 'Instruction'
data GenericInstruction (t :: (k -> *) -> [k] -> *) (o :: k -> *) where
  Instruction :: t o sh -> ShapedList o sh -> GenericInstruction t o

instance (E.TestEquality o, OrdF (t o), OrdF (ShapedList o)) => Ord (GenericInstruction t o) where
  Instruction op1 args1 `compare` Instruction op2 args2 =
    (Some op1, Some args1) `compare` (Some op2, Some args2)

-- -- | An implementation of heterogeneous lists for operands, with the
-- -- types of operands (caller-specified) reflected in the list type.
-- -- data OperandList f sh where
-- data OperandList :: (k -> *) -> [k] -> * where
--   Nil  :: OperandList f '[]
--   (:>) :: f tp -> OperandList f tps -> OperandList f (tp ': tps)

-- infixr 5 :>

-- instance (ShowF o) => Show (OperandList o sh) where
--   show Nil = "Nil"
--   show (elt :> rest) = showF elt ++ " :> " ++ show rest

-- instance (ShowF o) => ShowF (OperandList o)

-- instance (E.TestEquality o) => E.TestEquality (OperandList o) where
--   testEquality Nil Nil = Just E.Refl
--   testEquality (i1 :> rest1) (i2 :> rest2) =
--     case E.testEquality i1 i2 of
--       Just E.Refl ->
--         case E.testEquality rest1 rest2 of
--           Just E.Refl -> Just E.Refl
--           Nothing -> Nothing
--       Nothing -> Nothing
--   testEquality _ _ = Nothing

instance (E.TestEquality (c o), E.TestEquality o) => Eq (GenericInstruction c o) where
  Instruction o1 ops1 == Instruction o2 ops2 =
    case E.testEquality o1 o2 of
      Nothing -> False
      Just E.Refl ->
        case E.testEquality ops1 ops2 of
          Nothing -> False
          Just E.Refl -> True

instance (ShowF (c o), ShowF o) => Show (GenericInstruction c o) where
  show (Instruction opcode operands) =
    concat [ "Instruction "
           , showF opcode
           , " ("
           , showF operands
           , ")"
           ]

-- -- | A type parameterized map
-- mapOperandList :: (forall tp . a tp -> b tp) -> OperandList a sh -> OperandList b sh
-- mapOperandList f l =
--   case l of
--     Nil -> Nil
--     e :> rest -> f e :> mapOperandList f rest

-- mapOperandListIndexed :: forall a b sh
--                        . (forall tp . Index sh tp -> a tp -> b tp)
--                       -> OperandList a sh
--                       -> OperandList b sh
-- mapOperandListIndexed f = go id
--   where
--     go :: forall tps . (forall tp . Index tps tp -> Index sh tp)
--        -> OperandList a tps
--        -> OperandList b tps
--     go g l =
--       case l of
--         Nil -> Nil
--         e :> rest -> f (g IndexHere) e :> go (\ix -> g (IndexThere ix)) rest

-- traverseOperandList :: (Applicative t)
--                     => (forall tp . a tp -> t (b tp))
--                     -> OperandList a sh
--                     -> t (OperandList b sh)
-- traverseOperandList f l =
--   case l of
--     Nil -> pure Nil
--     e :> rest -> (:>) <$> f e <*> traverseOperandList f rest

-- traverseOperandListIndexed :: forall a b sh t
--                             . (Applicative t)
--                            => (forall tp . Index sh tp -> a tp -> t (b tp))
--                            -> OperandList a sh
--                            -> t (OperandList b sh)
-- traverseOperandListIndexed f = go id
--   where
--     go :: forall tps . (forall tp . Index tps tp -> Index sh tp)
--        -> OperandList a tps
--        -> t (OperandList b tps)
--     go g l =
--       case l of
--         Nil -> pure Nil
--         e :> rest -> (:>) <$> f (g IndexHere) e <*> go (\ix -> g (IndexThere ix)) rest

-- foldrOperandList :: forall sh a b . (forall tp . Index sh tp -> a tp -> b -> b) -> b -> OperandList a sh -> b
-- foldrOperandList f seed0 l = go id l seed0
--   where
--     go :: forall tps
--         . (forall tp . Index tps tp -> Index sh tp)
--        -> OperandList a tps
--        -> b
--        -> b
--     go g ops b =
--       case ops of
--         Nil -> b
--         a :> rest -> f (g IndexHere) a (go (\ix -> g (IndexThere ix)) rest b)

-- foldrMOperandList :: forall sh a b m . (Monad m)
--                   => (forall tp . Index sh tp -> a tp -> b -> m b)
--                   -> b
--                   -> OperandList a sh
--                   -> m b
-- foldrMOperandList f seed0 l = go id l seed0
--   where
--     go :: forall tps
--         . (forall tp . Index tps tp -> Index sh tp)
--        -> OperandList a tps
--        -> b
--        -> m b
--     go g ops b =
--       case ops of
--         Nil -> return b
--         a :> rest -> f (g IndexHere) a =<< go (\ix -> g (IndexThere ix)) rest b

-- -- | Return the number of operands in an operand list.
-- --
-- -- O(n)
-- operandListLength :: OperandList a sh -> Int
-- operandListLength Nil = 0
-- operandListLength (_ :> rest) = 1 + operandListLength rest

-- -- | Map over opcodes in a shape-preserving way
-- mapOpcode :: (forall (sh :: [k]) . c o sh -> c o sh) -> GenericInstruction c o -> GenericInstruction c o
-- mapOpcode f i =
--   case i of
--     Instruction op ops -> Instruction (f op) ops

-- | Map over opcodes while preserving the shape of the operand list, allowing effects
traverseOpcode :: (Applicative t)
               => (forall (sh :: [k]) . c o sh -> t (c o sh))
               -> GenericInstruction c o
               -> t (GenericInstruction c o)
traverseOpcode f i =
  case i of
    Instruction op ops -> Instruction <$> f op <*> pure ops

type OpcodeConstraints c o = (E.TestEquality (c o),
                              ShowF (c o),
                              EnumF (c o),
                              Typeable c,
                              Typeable o)

-- -- | Represents an index into a type-level list. Used in place of integers to
-- --   1. ensure that the given index *does* exist in the list
-- --   2. guarantee that it has the given kind
-- data Index :: [k] -> k -> * where
--   IndexHere :: forall x sh. Index (x ': sh) x
--   IndexThere :: forall x x' sh. Index sh x -> Index (x' ': sh) x
-- deriving instance Eq (Index sh x)
-- deriving instance Show (Index sh x)

-- instance ShowF (Index sh)

-- instance E.TestEquality (Index sh) where
--   IndexHere `testEquality` IndexHere = Just E.Refl
--   IndexThere idx1 `testEquality` IndexThere idx2 = E.testEquality idx1 idx2
--   _ `testEquality` _ = Nothing

-- instance OrdF (Index sh) where
--   IndexHere `compareF` IndexHere = EQF
--   IndexHere `compareF` IndexThere _ = LTF
--   IndexThere _ `compareF` IndexHere = GTF
--   IndexThere idx1 `compareF` IndexThere idx2 =
--     case idx1 `compareF` idx2 of
--       LTF -> LTF
--       EQF -> EQF
--       GTF -> GTF

-- instance Ord (Index sh x) where
--   x `compare` y = toOrdering $ x `compareF` y

-- -- | Evaluate an index for a given operand list.
-- indexOpList :: OperandList f sh -> Index sh s -> f s
-- -- Why not destructure @vals@ in the argument position? GHC gives a warning
-- -- about not handling the Nil case of vals. This way, GHC verifies that the
-- -- pattern-matching is exhaustive.
-- indexOpList vals IndexHere = case vals of x :> _ -> x
-- indexOpList vals (IndexThere th) = case vals of _ :> rest -> indexOpList rest th

-- indexAsInt :: Index sh tp -> Int
-- indexAsInt ix =
--   case ix of
--     IndexHere -> 0
--     IndexThere ix' -> 1 + indexAsInt ix'
