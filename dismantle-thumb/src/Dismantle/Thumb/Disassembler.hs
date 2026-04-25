{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.Thumb.Disassembler (
  disassembleInstruction
  ) where

import Dismantle.Thumb.ISA ( isa )
import Dismantle.Thumb.Opcodes
import Dismantle.ARM.Operands
import Dismantle.Thumb.Operands hiding ( Opcode )
import Dismantle.Tablegen.TH ( genDisassembler )

$(genDisassembler isa "data/ARM.tgen" ["data/overrides"])
