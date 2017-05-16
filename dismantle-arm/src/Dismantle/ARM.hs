{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.ARM (
  Instruction,
  AnnotatedInstruction,
  GenericInstruction(..),
  OperandList(..),
  Annotated(..),
  Operand(..),
  Opcode(..),
  mkPred,
  disassembleInstruction,
  assembleInstruction,
  ppInstruction
  )where

import Dismantle.ARM.ISA ( isa )
import Dismantle.Instruction
import Dismantle.Tablegen.TH ( genISA )
import Dismantle.ARM.Operands (mkPred)

$(genISA isa 'isa "data/ARM.tgen")
