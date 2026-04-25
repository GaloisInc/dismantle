{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file -dth-dec-file #-}
#if MIN_VERSION_base(4, 14, 0)
{-# OPTIONS_GHC -fbinary-blob-threshold=5000 #-}
#endif
module Dismantle.PPC.PrettyPrint (
  ppInstruction
  ) where

import Dismantle.PPC.ISA ( isa )
import Dismantle.PPC.Opcodes
import Dismantle.PPC.Operands
import Dismantle.Tablegen.TH ( genPrettyPrinter )

$(genPrettyPrinter isa "data/PPC.tgen" ["data/override"])
