module Dismantle.ASL.TH ( genISA ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Language.Haskell.TH as TH

import qualified Language.ASL.Parser as AP
import qualified Language.ASL.Syntax as AS
import qualified Dismantle.ASL as DA
import qualified Dismantle.Tablegen.TH as DTH

genISA :: (AS.InstructionEncoding -> Bool) -> DA.ISA -> FilePath -> TH.DecsQ
genISA fltr isa path = do
  txt <- TH.runIO (TIO.readFile path)
  case AP.parseAslInsts txt of
    Left err -> TH.reportError (T.unpack err) >> return []
    Right insns -> do
      let desc = DA.loadASL fltr (DA.isaName isa) insns
      DTH.genISADesc isa desc path []
