{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-spec-constr -fno-specialise -fmax-simplifier-iterations=1 -fno-call-arity #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file -dth-dec-file #-}
#if MIN_VERSION_base(4, 14, 0)
{-# OPTIONS_GHC -fbinary-blob-threshold=5000 #-}
#endif
module Dismantle.PPC.Disassembler (
  disassembleInstruction
  ) where

import Dismantle.PPC.ISA ( isa )
import Dismantle.PPC.Opcodes
import Dismantle.Tablegen.TH ( genDisassembler )

$(genDisassembler isa "data/PPC.tgen" ["data/override"])
