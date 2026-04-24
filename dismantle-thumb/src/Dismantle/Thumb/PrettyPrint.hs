{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-spec-constr -fno-specialise -fmax-simplifier-iterations=1 -fno-call-arity #-}
module Dismantle.Thumb.PrettyPrint (
  ppInstruction
  ) where

import Dismantle.Thumb.ISA ( isa )
import Dismantle.Thumb.Opcodes
import Dismantle.ARM.Operands
import Dismantle.Thumb.Operands hiding ( Opcode )
import Dismantle.Tablegen.TH ( genPrettyPrinter )

$(genPrettyPrinter isa "data/ARM.tgen" ["data/overrides"])
