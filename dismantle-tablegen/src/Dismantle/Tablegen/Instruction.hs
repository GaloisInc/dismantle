{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Dismantle.Tablegen.Instruction (
  Instruction(..),
  Annotated(..)
  ) where

import Dismantle.Tablegen.Shape

-- | A wrapper to allow operands to be easily annotated with arbitrary
-- data (of kind '*' for now).
--
-- Usage would be something like
--
-- > type AnnotatedInstruction = Instruction ISATag (Annotated OperandType AnnotationType)
--
-- The conversion to this type could be accomplished with a
-- parameterized fmap of the 'Annotated' constructor.  The annotation
-- is first so that a partial application during the initial fmap is
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
data Instruction (t :: (k -> *) -> Shape k -> *) (o :: k -> *) where
  Instruction :: t o sh -> OperandList o sh -> Instruction t o
