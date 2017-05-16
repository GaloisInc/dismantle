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
  disassembleInstruction,
  assembleInstruction,
  ppInstruction
  )where

import Dismantle.ARM.ISA ( isa )
import Dismantle.Instruction
import Dismantle.Tablegen.TH ( genISA )

$(genISA isa "data/ARM.tgen")
