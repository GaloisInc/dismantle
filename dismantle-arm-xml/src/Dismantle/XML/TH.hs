module Dismantle.XML.TH where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Language.Haskell.TH as TH
import qualified Text.XML.Light.Input as TX

import qualified Dismantle.XML as DX
import qualified Dismantle.Tablegen.TH as DTH

genISA :: DX.ISA -> FilePath -> TH.DecsQ
genISA isa path = do
  txt <- TH.runIO (TIO.readFile path)
  let content = TX.parseXML txt
  return undefined
