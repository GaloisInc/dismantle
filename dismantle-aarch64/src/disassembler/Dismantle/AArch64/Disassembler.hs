{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.AArch64.Disassembler (
  disassembleInstruction
  ) where

import Dismantle.AArch64.ISA ( isa )
import Dismantle.AArch64.Opcodes
import Dismantle.Tablegen.TH ( genDisassembler )

$(genDisassembler isa "data/AArch64.tgen" ["data/override"])
