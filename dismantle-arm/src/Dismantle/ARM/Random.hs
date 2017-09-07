{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.ARM.Random () where

import Dismantle.ARM
import Dismantle.ARM.ISA ( isa )
import Dismantle.Tablegen.TH ( genISARandomHelpers )

$(genISARandomHelpers isa "data/ARM.tgen")
