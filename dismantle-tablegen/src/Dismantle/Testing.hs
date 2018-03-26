{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provide some tools for testing disassemblers
module Dismantle.Testing (
  ArchTestConfig(..),
  Instruction(..),
  InstructionLayout(..),
  binaryTestSuite,
  withDisassembledFile
  ) where

import Data.Char ( intToDigit )
import Data.Maybe ( fromMaybe )
import Data.Word ( Word8, Word64 )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Monoid
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Traversable as T
import Numeric ( showIntAtBase )
import System.FilePath.Glob ( namesMatching )
import System.FilePath ( (</>) )
import qualified System.Process as Proc
import qualified Text.Megaparsec as P
import System.IO (hClose)
import qualified Text.RE.TDFA as RE
import qualified Text.PrettyPrint.HughesPJClass as PP
import Text.Printf ( printf )

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import Dismantle.Testing.Parser
import Dismantle.Tablegen.ISA (ISA(isaInputEndianness, isaName), Endianness(Little))

import Prelude

-- | Configuration to drive the shared testing infrastructure
data ArchTestConfig = forall i .
  ATC { testingISA :: ISA
      -- ^ The ISA associated with the test input
      , disassemble :: LBS.ByteString -> (Int, Maybe i)
      -- ^ The disassembly function for the ISA
      , assemble :: i -> LBS.ByteString
      -- ^ The re-assembly function for the ISA
      , prettyPrint :: i -> PP.Doc
      -- ^ The pretty printer for the ISA
      , expectFailure :: Maybe RE.RE
      -- ^ A regular expression run against the text of an instruction (from
      -- objdump); if the regular expression matches, disassembly and reassembly
      -- are expected to fail (and the pretty printing check is therefore not
      -- run).
      , instructionFilter :: Instruction -> Bool
      -- ^ A function to determine which parsed instructions should be
      -- tested.
      , skipPrettyCheck :: Maybe RE.RE
      -- ^ A regular expression run against the text of an instruction (from
      -- objdump); if the regular expression matches, the output of the pretty
      -- printer is not compared against the original text provided by objdump.
      , ignoreAddresses :: [(FilePath, [Word64])]
      -- ^ A list of files and addresses in those files to ignore. This
      -- is typically used when we know that some locations contain data
      -- bytes and we don't want to test instruction parses of those
      -- bytes.
      , customObjdumpArgs :: [(FilePath, [String])]
      -- ^ Custom arguments to objdump to disassemble the specified file.
      -- Files not present in this mapping will be disassembled with
      -- default objdump arguments. Entries in this mapping must provide
      -- all arguments to objdump up to but not including the file name,
      -- so this includes the disassembly flag (-d/-D).
      , normalizePretty :: TL.Text -> TL.Text
      -- ^ A function to normalize a pretty-printed instruction to a
      -- form suitable for comparison. This typically needs to remove
      -- whitespace and special characters whose presence confounds
      -- pretty-print comparisons but is otherwise unimportant for
      -- comparison purposes.
      }

addressIsIgnored :: ArchTestConfig -> FilePath -> Word64 -> Bool
addressIsIgnored atc file addr =
    case lookup file (ignoreAddresses atc) of
        Nothing -> False
        Just addrs -> addr `elem` addrs

-- | Given an architecture-specific configuration and a directory containing
-- binaries, run @objdump@ on each binary and then try to disassemble and
-- re-assemble each instruction in those binaries (as identified by @objdump@).
--
-- Additionally, the output of the automatically-generated pretty printer is
-- compared against the output of @objdump@ unless the 'skipPrettyCheck' regex
-- matches the instruction under test.
binaryTestSuite :: ArchTestConfig -> FilePath -> IO T.TestTree
binaryTestSuite atc dir = do
  binaries <- namesMatching (dir </> "*")
  tests <- mapM (mkDisassembledBinaryTest atc) binaries
  return (T.testGroup (isaName (testingISA atc)) tests)

mkDisassembledBinaryTest :: ArchTestConfig -> FilePath -> IO T.TestTree
mkDisassembledBinaryTest atc binaryPath = do
  return $ T.testCaseInfo binaryPath $ do
    let fileCustomArgs = lookup binaryPath (customObjdumpArgs atc)
    let filterInstruction = instructionFilter atc
    withDisassembledFile objdumpParser fileCustomArgs binaryPath $ \d -> do
      let insns = filter filterInstruction (concatMap instructions (sections d))
      testAgg <- F.foldrM (testInstruction atc binaryPath) emptyTestAggregate insns
      let ok = and [ null (testDisassemblyFailures testAgg)
                   , null (testRoundtripFailures testAgg)
                   , null (testPrettyFailures testAgg)
                   ]
      T.assertBool (formatTestFailure testAgg) ok
      return (printf "%s (%d/%d - tests/expected failures)" binaryPath (testCount testAgg) (testExpectedFailure testAgg))

testInstruction :: ArchTestConfig -> FilePath -> Instruction -> TestAggregate -> IO TestAggregate
testInstruction atc binaryPath i agg
  | addressIsIgnored atc binaryPath (insnAddress i) = return agg
  | otherwise =
    case atc of
      ATC { disassemble = disasm
          , assemble = asm
          , prettyPrint = pp
          , skipPrettyCheck = skipPPRE
          , expectFailure = expectFailureRE
          , normalizePretty = norm
          } -> case maybe False (insnText i RE.=~) expectFailureRE of
                 False -> testInstructionWith norm disasm asm pp skipPPRE i agg
                 True -> return (agg { testExpectedFailure = testExpectedFailure agg + 1
                                     , testCount = testCount agg + 1
                                     })

testInstructionWith :: (TL.Text -> TL.Text)
                    -> (LBS.ByteString -> (Int, Maybe i))
                    -> (i -> LBS.ByteString)
                    -> (i -> PP.Doc)
                    -> Maybe RE.RE
                    -> Instruction
                    -> TestAggregate
                    -> IO TestAggregate
testInstructionWith norm disasm asm pp skipPPRE i agg = do
  let (_consumed, minsn) = disasm (insnBytes i)
  case minsn of
    Nothing -> return (agg { testDisassemblyFailures = i : testDisassemblyFailures agg
                           , testCount = testCount agg + 1
                           })
    Just insn -> do
      case insnBytes i == asm insn of
        False -> do
          let failure = (i, show (pp insn), binaryRep (insnBytes i), binaryRep (asm insn))
          return (agg { testRoundtripFailures = failure : testRoundtripFailures agg
                      , testCount = testCount agg + 1
                      })
        True
          | not (maybe False (insnText i RE.=~) skipPPRE) ->
            case norm (insnText i) == norm (TL.pack (show (pp insn))) of
              True -> return (agg { testCount = testCount agg + 1 })
              False -> do
                let failure = (i, show (pp insn))
                return (agg { testPrettyFailures = failure : testPrettyFailures agg
                            , testCount = testCount agg + 1
                            })
          | otherwise -> return (agg { testCount = testCount agg + 1 })

data TestAggregate =
  TestAggregate { testDisassemblyFailures :: [Instruction]
                , testRoundtripFailures :: [(Instruction, String, String, String)]
                , testPrettyFailures :: [(Instruction, String)]
                , testCount :: !Int
                , testExpectedFailure :: !Int
                }

emptyTestAggregate :: TestAggregate
emptyTestAggregate = TestAggregate { testDisassemblyFailures = []
                                   , testRoundtripFailures = []
                                   , testPrettyFailures = []
                                   , testCount = 0
                                   , testExpectedFailure = 0
                                   }

formatTestFailure :: TestAggregate -> String
formatTestFailure ta = show doc
  where
    doc = PP.vcat [ "Disassembly failures:"
                  , PP.nest 2 (PP.vcat disasmFailures)
                  , "Roundtrip failures:"
                  , PP.nest 2 (PP.vcat roundtripFailures)
                  , "Pretty printing failures:"
                  , PP.nest 2 (PP.vcat prettyFailures)
                  ]
    disasmFailures = [ PP.text (printf "Failed to disassemble %s (%s)" (binaryRep (insnBytes i)) (TL.unpack (insnText i)))
                     | i <- testDisassemblyFailures ta
                     ]
    roundtripFailures = [ PP.text (printf "Roundtrip %s (parsed as %s):\n\tOriginal Bytes: %s\n\tReassembled as: %s" (show (insnText i)) (show parsedAs) (show origBytes) (show reassembledBytes))
                        | (i, parsedAs, origBytes, reassembledBytes) <- testRoundtripFailures ta
                        ]
    prettyFailures = [ PP.text (printf "Pretty printing comparison failed (bytes: %s)\n\tExpected: '%s'\n\tActual:   '%s' " (binaryRep (insnBytes i)) (insnText i) actual)
                     | (i, actual) <- testPrettyFailures ta
                     ]

-- | Convert a directory of executables into a list of data, where
-- each data item is constructed by a callback called on one
-- instruction disassembled by objdump.
withInstructions :: ArchTestConfig
                 -- ^ The architecture testing configuration
                 -> Parser Disassembly
                 -- ^ The parser to use to parse the objdump output
                 -> FilePath
                 -- ^ A directory containing executables (that can be objdumped)
                 -> [(FilePath, [String])]
                 -- ^ Custom objdump disassembly arguments for binaries
                 -- that need them
                 -> (Instruction -> Bool)
                 -- ^ Instruction filter
                 -> (FilePath -> Word64 -> LBS.ByteString -> TL.Text -> a)
                 -- ^ Turn a disassembled instruction into data
                 -> IO [(FilePath, [a])]
withInstructions atc parser dir customArgs filterInstruction con = do
  files <- namesMatching (dir </> "*")
  mapM disassembleFile files
  where
    disassembleFile f = do
      let fileCustomArgs = lookup f customArgs
      insns <- withDisassembledFile parser fileCustomArgs f $ \d -> do

        -- We assume that objdump always produces instruction
        -- bytestrings in big-endian format. If the ISA expects
        -- little-endian input, we need to rewrite the test case inputs.
        let rewriteDisassembly = case isaInputEndianness (testingISA atc) of
              Little swapBytes _ -> fmap (rewriteSection swapBytes)
              _ -> id
            rewriteSection fn s = s { instructions = rewriteInstruction fn <$> instructions s }
            rewriteInstruction fn i = i { insnBytes = fn $ insnBytes i }
            theSections = rewriteDisassembly $ sections d

        T.forM (filter filterInstruction $ concatMap instructions theSections) $ \i ->
            return (con f (insnAddress i) (insnBytes i) (insnText i))
      return (f, insns)

withDisassembledFile :: Parser Disassembly -> Maybe [String] -> FilePath -> (Disassembly -> IO a) -> IO a
withDisassembledFile parser customArgs f k = do
  (_, Just hout, _, ph) <- Proc.createProcess p1
  t <- TL.hGetContents hout
  case P.runParser parser f t of
    Left err -> do
      hClose hout
      _ <- Proc.waitForProcess ph
      error $ P.parseErrorPretty err
    Right d -> do
      res <- k d
      hClose hout
      _ <- Proc.waitForProcess ph
      return res
  where
    p0 = Proc.proc "objdump" args
    args = (fromMaybe defaultArgs customArgs) <> [f]
    defaultArgs = ["-d"]
    p1 = p0 { Proc.std_out = Proc.CreatePipe
            }

showByte :: Word8 -> String
showByte b =
    let s = showIntAtBase 2 intToDigit b ""
        padding = replicate (8 - length s) '0'
    in padding <> s

binaryRep :: LBS.ByteString -> String
binaryRep bytes = L.intercalate "." $ showByte <$> LBS.unpack bytes
