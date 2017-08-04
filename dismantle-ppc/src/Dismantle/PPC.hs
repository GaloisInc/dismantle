{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-spec-constr -fno-specialise -fmax-simplifier-iterations=1 -fno-call-arity #-}
module Dismantle.PPC (
  Instruction,
  AnnotatedInstruction,
  GenericInstruction(..),
  ShapedList(..),
  Annotated(..),
  Operand(..),
  Opcode(..),
  module Dismantle.PPC.Operands,
  disassembleInstruction,
  assembleInstruction,
  ppInstruction
  )where

import Data.Parameterized.ShapedList ( ShapedList(..) )

import Dismantle.Instruction
import Dismantle.PPC.Operands
import Dismantle.PPC.ISA ( isa )
import Dismantle.Tablegen.TH ( genISA )

$(genISA isa "data/PPC.tgen")
