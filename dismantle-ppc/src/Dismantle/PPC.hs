{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-spec-constr -fno-specialise -fmax-simplifier-iterations=1 -fno-call-arity #-}
-- Dump TH splices to two files on disk. The generated file
-- Dismantle/PPC.dump-splices will contain all splices, and not be
-- valid Haskell, while the generated file Dismantle/PPC.th.hs will
-- have only the top-level splices, and will be valid Haskell. The
-- second file can be used when generating TAGS.
{-# OPTIONS_GHC -ddump-splices -ddump-to-file -dth-dec-file #-}
module Dismantle.PPC (
  Instruction,
  AnnotatedInstruction,
  GenericInstruction(..),
  List(..),
  Annotated(..),
  Operand(..),
  OperandRepr(..),
  operandReprString,
  Opcode(..),
  module Dismantle.PPC.Operands,
  disassembleInstruction,
  assembleInstruction,
  ppInstruction
  )where

import Data.Parameterized.List ( List(..) )

import Dismantle.Instruction
import Dismantle.PPC.Operands
import Dismantle.PPC.ISA ( isa )
import Dismantle.Tablegen.TH ( genISA, genInstances )

$(genISA isa "data/PPC.tgen" [])
$(return [])

-- We need a separate call to generate some instances, since the helper(s) that
-- create these instances use reify, which we can't call until we flush the TH
-- generation using the @$(return [])@ trick.
$(genInstances)
