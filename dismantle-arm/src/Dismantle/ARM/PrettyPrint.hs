{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.ARM.PrettyPrint (
  ppInstruction
  ) where

import Dismantle.ARM.ISA ( isa )
import Dismantle.ARM.Opcodes
import Dismantle.ARM.Operands
import Dismantle.Tablegen.TH ( genPrettyPrinter )

$(genPrettyPrinter isa "data/ARM.tgen" [])
