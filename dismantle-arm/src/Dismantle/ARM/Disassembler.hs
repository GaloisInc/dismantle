{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.ARM.Disassembler (
  disassembleInstruction
  ) where

import Dismantle.ARM.ISA ( isa )
import Dismantle.ARM.Opcodes
import Dismantle.ARM.Operands
import Dismantle.Tablegen.TH ( genDisassembler )

$(genDisassembler isa "data/ARM.tgen" [])
