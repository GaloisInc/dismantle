{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.PPC where

import Dismantle.Tablegen.ISA ( ppc )
import Dismantle.Tablegen.TH ( genISA )

$(genISA ppc 'ppc "data/PPC.tgen")
