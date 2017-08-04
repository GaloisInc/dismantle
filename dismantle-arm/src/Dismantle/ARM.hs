{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-spec-constr -fno-specialise -fmax-simplifier-iterations=1 -fno-call-arity #-}
module Dismantle.ARM (
  Instruction,
  AnnotatedInstruction,
  GenericInstruction(..),
  ShapedList(..),
  Annotated(..),
  Operand(..),
  Opcode(..),
  mkPred,
  disassembleInstruction,
  assembleInstruction,
  ppInstruction
  )where

import Data.Parameterized.ShapedList ( ShapedList(..) )

import Dismantle.ARM.ISA ( isa )
import Dismantle.Instruction
import Dismantle.Tablegen.TH ( genISA )
import Dismantle.ARM.Operands (mkPred)

$(genISA isa "data/ARM.tgen")
