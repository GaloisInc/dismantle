{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.Thumb.Random () where

import Dismantle.Thumb.Opcodes
import Dismantle.Thumb.Operands hiding ( Opcode )
import Dismantle.Thumb.ISA ( isa )
import Dismantle.Tablegen.TH ( genISARandomHelpers )

$(genISARandomHelpers isa "data/ARM.tgen" ["data/overrides"])
