module Dismantle.ARM.TH where


import           Control.Monad ( mapM_ )
import qualified Data.Foldable as F
import           Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Traversable as T
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import           System.IO ( withFile, IOMode(..), hPutStrLn )
import           System.Directory (withCurrentDirectory, listDirectory, makeAbsolute)
import           System.FilePath.Posix (isExtensionOf)
import           System.FilePath.Glob ( namesMatching )
import           System.FilePath ( (</>), (<.>) )

import qualified Text.XML.Light as X

import qualified Dismantle.Tablegen as DT
import qualified Dismantle.ARM.XML as XML
import qualified Dismantle.ARM.ASL as ASL
import qualified Dismantle.ARM.ASL as ARM ( encodingOpToInstDescriptor, instDescriptorsToISA )
import qualified Dismantle.Tablegen.TH as DTH

genISA ::  DT.ISA -> FilePath -> FilePath -> FilePath -> FilePath -> TH.DecsQ
genISA isa xmldirPath encIndexFile aslInstrs logFile = do
  xmlFiles <- TH.runIO $ withCurrentDirectory xmldirPath $ namesMatching ("*" <.> "xml")
  desc <- TH.runIO $ withFile logFile WriteMode $ \handle -> do
    let doLog msg = hPutStrLn handle msg
    putStrLn $ "Dismantle.ARM.TH log: " ++ logFile
    encodings <- withCurrentDirectory xmldirPath $ do
      XML.loadEncodings (DT.isaName isa) xmlFiles encIndexFile doLog
    encodingops <- ASL.loadASL (DT.isaName isa) aslInstrs encodings doLog
    return $ ARM.instDescriptorsToISA $ map ARM.encodingOpToInstDescriptor encodingops
  TH.runIO $ putStrLn "Successfully generated ISA description."
  let files = aslInstrs : map ((</>) xmldirPath) xmlFiles
  DTH.genISADesc isa desc files
