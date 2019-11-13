module Dismantle.ARM.TH where

import qualified Data.Foldable as F
import           Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Traversable as T
import qualified Language.Haskell.TH as TH
import           System.Directory (withCurrentDirectory, listDirectory)
import           System.FilePath.Posix (isExtensionOf)

import qualified Text.XML.Light as X

import qualified Dismantle.ARM as DA
import qualified Dismantle.Tablegen.TH as DTH

genISA :: (X.Element -> Bool) -> DA.ISA -> [FilePath] -> FilePath -> TH.DecsQ
genISA fltr isa encFileNames dirPath = do
  (desc, xmlFiles) <- TH.runIO $ withCurrentDirectory dirPath $ do
    files <- sort <$> listDirectory "."
    let xmlFiles = filter ("xml" `isExtensionOf`) files
    desc <- F.fold <$> (T.forM encFileNames $ \encFileName -> DA.loadXML fltr (DA.isaName isa) encFileName ".")
    return (desc, xmlFiles)
  TH.runIO $ putStrLn "Successfully generated ISA description."
  DTH.genISADesc isa desc (((dirPath ++ "/") ++) <$> xmlFiles)
