module Dismantle.XML.TH where

import           Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Language.Haskell.TH as TH
import           System.Directory (withCurrentDirectory, listDirectory)
import           System.FilePath.Posix (isExtensionOf)

import qualified Text.XML.Light as X

import qualified Dismantle.XML as DX
import qualified Dismantle.Tablegen.TH as DTH

genISA :: (X.Element -> Bool) -> DX.ISA -> FilePath -> TH.DecsQ
genISA fltr isa dir = do
  (desc, xmlFiles) <- TH.runIO $ withCurrentDirectory dir $ do
    files <- sort <$> listDirectory "."
    let xmlFiles = filter ("xml" `isExtensionOf`) files
    desc <- DX.loadXML fltr (DX.isaName isa) "."
    return (desc, xmlFiles)
  TH.runIO $ putStrLn "Successfully generated ISA description."
  DTH.genISADesc isa desc (((dir ++ "/") ++) <$> xmlFiles) []
