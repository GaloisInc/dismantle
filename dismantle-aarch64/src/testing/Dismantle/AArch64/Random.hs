{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.AArch64.Random () where

import Dismantle.AArch64.Opcodes
import Dismantle.AArch64.Operands
import Dismantle.AArch64.ISA ( isa )
import Dismantle.Tablegen.TH ( genISARandomHelpers )

$(genISARandomHelpers isa "data/AArch64.tgen" ["data/override"])
