{-# LANGUAGE DataKinds #-}
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
  disassembleInstruction,
  assembleInstruction,
  ppInstruction
  )where

import Dismantle.ARM.ISA ( isa )
import Dismantle.Tablegen.TH ( genISA )
import Dismantle.Tablegen.Instruction

$(genISA isa 'isa "data/ARM.tgen")