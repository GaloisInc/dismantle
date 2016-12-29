{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Dismantle.Tablegen.TH (
  genISA
  ) where
import GHC.TypeLits ( Symbol )
import Language.Haskell.TH

import Dismantle.Tablegen.Instruction
import Dismantle.Tablegen.Types

genISA :: ISADescriptor -> DecsQ
genISA = undefined

{-

For each ISA, we have to generate:

1) A datatype representing all possible operands (along with an
associated tag type, one tag for each operand type).  There may be
some sub-types (e.g., a separate Register type to be a parameter to a
Reg32 operand).

2) An ADT representing all possible instructions - this is a simple
GADT with no parameters, but the return types are lists of the types
of the operands for the instruction represented by the tag.

3) A type alias instantiating the underlying Instruction type with the
Tag and Operand types.

4) A pretty printer

5) A parser

6) An unparser

-}

data OpTag = Imm32
           | Reg32

data Operand (tp :: Symbol) where
  OImm32 :: Int -> Operand "Imm32"
  OReg32 :: Int -> Operand "Reg32"

s2 :: OperandList Operand '["Imm32", "Reg32"]
s2 = OImm32 5 :> OReg32 0 :> Nil

s3 = case s2 of
  OImm32 _ :> l -> l

insn = Instruction Add s2

data ISATag o sh where
  Add :: ISATag Operand '["Imm32", "Reg32"]
  Sub :: ISATag Operand '["Imm32", "Reg32"]

type I = Instruction ISATag Operand
type AnnotatedI = Instruction ISATag (Annotated () Operand)

foo :: I -> Int
foo i =
  case i of
    Instruction Add (OImm32 imm :> OReg32 regNo :> Nil) -> imm + regNo
