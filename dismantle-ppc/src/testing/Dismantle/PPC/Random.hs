{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.PPC.Random () where

import Dismantle.PPC.Opcodes
import Dismantle.PPC.Operands
import Dismantle.PPC.ISA ( isa )
import Dismantle.Tablegen.TH ( genISARandomHelpers )

$(genISARandomHelpers isa "data/PPC.tgen" ["data/override"])
