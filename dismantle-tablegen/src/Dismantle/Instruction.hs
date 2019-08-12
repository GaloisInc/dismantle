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
  GenericInstruction(..),
  Annotated(..),
  OpcodeConstraints,
  traverseOpcode
  ) where

import Control.Monad
import qualified Data.Type.Equality as E
import Data.Typeable ( Typeable )

import Data.EnumF ( EnumF(..) )
import Data.Parameterized.Classes ( ShowF(..), EqF(..), OrdF(..) )
import Data.Parameterized.Some ( Some(..) )
import Data.Parameterized.List ( List )

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

instance (Eq a, E.TestEquality o) => E.TestEquality (Annotated a o) where
  testEquality (Annotated a otp) (Annotated a' otp') = guard (a == a') >> E.testEquality otp otp'

instance (Eq a, EqF o) => Eq (Annotated a o tp) where
  Annotated a otp == Annotated a' otp' = a == a' && eqF otp otp'

instance (Show a, ShowF o) => Show (Annotated a o tp) where
  show (Annotated a o) = unwords [ "Annotated", show a, " ", showF o ]

instance (Show a, ShowF o) => ShowF (Annotated a o)

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
  Instruction :: t o sh -> List o sh -> GenericInstruction t o

instance (E.TestEquality o, OrdF (t o), OrdF (List o)) => Ord (GenericInstruction t o) where
  Instruction op1 args1 `compare` Instruction op2 args2 =
    (Some op1, Some args1) `compare` (Some op2, Some args2)

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
