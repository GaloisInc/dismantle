{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-spec-constr -fno-specialise -fmax-simplifier-iterations=1 -fno-call-arity #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file -dth-dec-file #-}
#if MIN_VERSION_base(4, 14, 0)
{-# OPTIONS_GHC -fbinary-blob-threshold=5000 #-}
#endif
module Dismantle.PPC.Opcodes (
  Instruction,
  AnnotatedInstruction,
  GenericInstruction(..),
  List(..),
  Annotated(..),
  Operand(..),
  OperandRepr(..),
  operandReprString,
  Opcode(..),
  getInstructionTag
  ) where

import Data.Parameterized.List ( List(..) )

import Dismantle.Instruction
import Dismantle.PPC.Operands
import Dismantle.PPC.ISA ( isa )
import Dismantle.Tablegen.TH ( genOpcodeTypes, genInstances )

$(genOpcodeTypes isa "data/PPC.tgen" ["data/override"])
$(return [])

-- We need a separate call to generate some instances, since the helper(s) that
-- create these instances use reify, which we can't call until we flush the TH
-- generation using the @$(return [])@ trick.
$(genInstances)
