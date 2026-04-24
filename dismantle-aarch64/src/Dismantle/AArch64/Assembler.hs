{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-spec-constr -fno-specialise -fmax-simplifier-iterations=1 -fno-call-arity #-}
module Dismantle.AArch64.Assembler (
  assembleInstruction
  ) where

import Dismantle.AArch64.ISA ( isa )
import Dismantle.AArch64.Opcodes
import Dismantle.Tablegen.TH ( genAssembler )

$(genAssembler isa "data/AArch64.tgen" ["data/override"])
