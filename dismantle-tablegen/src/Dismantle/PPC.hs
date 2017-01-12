{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.PPC (
  Instruction,
  AnnotatedInstruction,
  GenericInstruction(..),
  OperandList(..),
  Annotated(..),
  Operand(..),
  Opcode(..),
  module Dismantle.PPC.Operands,
  disassembleInstruction,
  assembleInstruction,
  ppInstruction
  )where

import Dismantle.PPC.Operands
import Dismantle.Tablegen.ISA ( ppc )
import Dismantle.Tablegen.TH ( genISA )
import Dismantle.Tablegen.Instruction

$(genISA ppc 'ppc "data/PPC.tgen")

