{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-spec-constr -fno-specialise -fmax-simplifier-iterations=1 -fno-call-arity #-}
module Dismantle.ARM.Disassembler (
  disassembleInstruction
  ) where

import Dismantle.ARM.ISA ( isa )
import Dismantle.ARM.Opcodes
import Dismantle.ARM.Operands
import Dismantle.Tablegen.TH ( genDisassembler )

$(genDisassembler isa "data/ARM.tgen" [])
