{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Dismantle.Instruction (
  OperandList(..),
  GenericInstruction(..),
  Annotated(..),
  OpcodeConstraints,
  SomeOpcode(..),
  mapOpcode,
  traverseOpcode,
  mapOperandList
  ) where

import qualified Data.Type.Equality as E
import Data.Typeable ( Typeable )

import Data.EnumF ( EnumF(..) )
import Data.ShowF ( ShowF(..) )

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
  Instruction :: t o sh -> OperandList o sh -> GenericInstruction t o

-- | An implementation of heterogeneous lists for operands, with the
-- types of operands (caller-specified) reflected in the list type.
-- data OperandList f sh where
data OperandList :: (k -> *) -> [k] -> * where
  Nil  :: OperandList f '[]
  (:>) :: f tp -> OperandList f tps -> OperandList f (tp ': tps)

infixr 5 :>

-- | A type parameterized map
mapOperandList :: (forall tp . a tp -> b tp) -> OperandList a sh -> OperandList b sh
mapOperandList f l =
  case l of
    Nil -> Nil
    e :> rest -> f e :> mapOperandList f rest

-- | Map over opcodes in a shape-preserving way
mapOpcode :: (forall (sh :: [k]) . c o sh -> c o sh) -> GenericInstruction c o -> GenericInstruction c o
mapOpcode f i =
  case i of
    Instruction op ops -> Instruction (f op) ops

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

-- | A wrapper around an opcode tag that hides the shape parameter.
--
-- This allows opcodes to be stored heterogeneously in data structures.  Pattern
-- matching on them and using 'E.testEquality' allows the shape to be recovered.
data SomeOpcode (c :: (k -> *) -> [k] -> *) (o :: k -> *) = forall (sh :: [k]) . SomeOpcode (c o sh)

instance (ShowF (c o)) => Show (SomeOpcode c o) where
  show (SomeOpcode o) = showF o

instance (E.TestEquality (c o)) => Eq (SomeOpcode c o) where
  SomeOpcode o1 == SomeOpcode o2 =
    case E.testEquality o1 o2 of
      Just E.Refl -> True
      Nothing -> False

instance (E.TestEquality (c o), EnumF (c o)) => Ord (SomeOpcode c o) where
  SomeOpcode o1 `compare` SomeOpcode o2 = enumF o1 `compare` enumF o2
