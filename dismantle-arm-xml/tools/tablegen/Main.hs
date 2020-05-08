module Main ( main ) where

import qualified Codec.Compression.GZip as CCG
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.Async as CCA
import qualified Data.Binary as DB
import qualified Data.ByteString.Lazy as LBS
import qualified Language.Haskell.TH as LTH
import qualified Options.Applicative as O
import qualified System.FilePath as SF
import qualified System.IO as IO

import qualified Dismantle.ARM.ISA as DAI
import qualified Dismantle.ARM.TH as DAT
import qualified Dismantle.Tablegen.ISA as DTI
import qualified Dismantle.Tablegen.TH as DTH
import qualified Dismantle.Tablegen.BitTrie as BT
import qualified Dismantle.Tablegen.LinearizedTrie as LT

t32ISA :: DTI.ISA
t32ISA = DAI.isa "T32"

a32ISA :: DTI.ISA
a32ISA = DAI.isa "A32"

generateParseTable :: DTI.ISA
                   -> FilePath
                   -> FilePath
                   -> FilePath
                   -> IO.Handle
                   -> IO LBS.ByteString
generateParseTable isa xmldirPath encIndexFile aslInstrs logHandle = do
  (isaDesc, _, files) <- DAT.armISADesc isa xmldirPath encIndexFile aslInstrs logHandle
  parserData <- LTH.runQ $ mapM (DTH.mkTrieInput isa) (DTH.parsableInstructions isa isaDesc)
  let (trieInputs, _) = unzip parserData
  case BT.bitTrie Nothing trieInputs of
    Left err -> error (show err)
    Right bt0 -> do
      -- Convert TH names to strings
      --
      -- This is lossy, but we'll make sure it is safe when we read it back in
      let lt = fmap (fmap LTH.nameBase) bt0
      deps <- mapM LBS.readFile files
      return (LT.serialize DB.put deps lt)

data Options =
  Options { oParallel :: Bool
          }

options :: O.Parser Options
options = Options <$> O.switch ( O.long "parallel"
                               <> O.short 'p'
                               <> O.help "Generate tables in parallel"
                               )

main :: IO ()
main = genWithOpts =<< O.execParser opts
  where
    opts = O.info (options O.<**> O.helper)
           ( O.fullDesc
           <> O.progDesc "A tool for generating ARM parse tables"
           <> O.header "dismantle-arm-xml-tablegen - Parse table generator"
           )

data ARMSet = Lite | Full
  deriving (Show)

data ISAInfo =
  ISAInfo { infoIsa :: DTI.ISA
          , infoARMSet :: ARMSet
          , infoXMLPath :: FilePath
          , infoEncIndex :: FilePath
          , infoASLInstrs :: FilePath
          }

isaInfo :: [ISAInfo]
isaInfo = [ ISAInfo t32ISA Lite "data/ISA_uboot_req" "t32_encindex.xml" "data/Parsed/arm_instrs.sexpr"
          , ISAInfo t32ISA Full "data/ISA_v85A_AArch32_xml_00bet9" "t32_encindex.xml" "data/Parsed/arm_instrs.sexpr"
          , ISAInfo a32ISA Lite "data/ISA_uboot_req" "a32_encindex.xml" "data/Parsed/arm_instrs.sexpr"
          , ISAInfo a32ISA Full "data/ISA_v85A_AArch32_xml_00bet9" "a32_encindex.xml" "data/Parsed/arm_instrs.sexpr"
          ]

persistTables :: ISAInfo -> IO ()
persistTables info = IO.withFile logFileName IO.WriteMode $ \handle -> do
  bytes <- generateParseTable (infoIsa info) (infoXMLPath info) (infoEncIndex info) (infoASLInstrs info) handle
  LBS.writeFile serFileName (CCG.compress bytes)
  where
    baseFileName = concat [ DTI.isaName (infoIsa info)
                          , "-"
                          , show (infoARMSet info)
                          ]
    logFileName = "data" SF.</> baseFileName SF.<.> "log"
    serFileName = "data" SF.</> baseFileName SF.<.> "bin"

genWithOpts :: Options -> IO ()
genWithOpts opts = do
  -- We always claim 4 capabilities if asked to compute in parallel; we only
  -- have four jobs total.  It would be nice to have a proper work-stealing
  -- queue to handle a lower number.
  case oParallel opts of
    False -> mapM_ persistTables isaInfo
    True -> do
      CC.setNumCapabilities 4
      CCA.mapConcurrently_ persistTables isaInfo
