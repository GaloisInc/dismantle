{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Dismantle.Tablegen.Instruction (
  Instruction(..)
  ) where

import Dismantle.Tablegen.Shape

-- | The type of instructions
--
-- This type is parameterized by a *tag* type, which represents all of
-- the instructions in an ISA.  For example:
--
-- > data Tag s where
-- >   Add :: Tag (EmptyShape '::> OImm32 ::> OReg32
--
-- Could be an ISA with one instruction that adds a 32 bit immediate
-- to the value held in a 32 bit register (for suitable definitions of
-- OImm32 and OReg32).
data Instruction (t :: (k -> *) -> Shape k -> *) (o :: k -> *) where
  Instruction :: t o s -> OperandList o s -> Instruction t o
