{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-spec-constr -fno-specialise -fmax-simplifier-iterations=1 -fno-call-arity #-}
module Dismantle.ARM.PrettyPrint (
  ppInstruction
  ) where

import Dismantle.ARM.ISA ( isa )
import Dismantle.ARM.Opcodes
import Dismantle.ARM.Operands
import Dismantle.Tablegen.TH ( genPrettyPrinter )

$(genPrettyPrinter isa "data/ARM.tgen" [])
