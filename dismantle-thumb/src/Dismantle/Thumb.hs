{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-spec-constr -fno-specialise -fmax-simplifier-iterations=1 -fno-call-arity #-}
module Dismantle.Thumb (
  Instruction,
  AnnotatedInstruction,
  GenericInstruction(..),
  List(..),
  Annotated(..),
  Operand(..),
  OperandRepr(..),
  operandReprString,
  Opcode(..),
  mkPred,
  disassembleInstruction,
  assembleInstruction,
  ppInstruction
  )where

import Data.Parameterized.List ( List(..) )

import Dismantle.Thumb.ISA ( isa )
import Dismantle.Instruction
import Dismantle.Tablegen.TH ( genISA, genInstances )
import Dismantle.Thumb.Operands (mkPred)

$(genISA isa "data/ARM.tgen" ["data/overrides"])
$(return [])

-- We need a separate call to generate some instances, since the helper(s) that
-- create these instances use reify, which we can't call until we flush the TH
-- generation using the @$(return [])@ trick.
$(genInstances)
