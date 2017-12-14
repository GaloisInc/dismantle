{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-spec-constr -fno-specialise -fmax-simplifier-iterations=1 -fno-call-arity #-}
module Dismantle.AArch64 (
  Instruction,
  AnnotatedInstruction,
  GenericInstruction(..),
  List(..),
  Annotated(..),
  Operand(..),
  Opcode(..),
  mkPred,
  disassembleInstruction,
  assembleInstruction,
  ppInstruction
  )where

import Data.Parameterized.List ( List(..) )

import Dismantle.ARM (mkPred)
import Dismantle.AArch64.ISA ( isa )
import Dismantle.Instruction
import Dismantle.Tablegen.TH ( genISA, genInstances )

$(genISA isa "data/AArch64.tgen" ["data/override"])
$(return [])

-- We need a separate call to generate some instances, since the helper(s) that
-- create these instances use reify, which we can't call until we flush the TH
-- generation using the @$(return [])@ trick.
$(genInstances)
