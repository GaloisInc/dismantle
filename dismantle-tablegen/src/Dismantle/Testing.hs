{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provide some tools for testing disassemblers
module Dismantle.Testing (
  ArchTestConfig(..),
  Instruction(..),
  InstructionLayout(..),
  binaryTestSuite,
  mkTestCase,
  withInstructions,
  withDisassembledFile
  ) where

import Control.Monad ( unless )
import Data.Char ( intToDigit )
import Data.Maybe ( catMaybes )
import Data.Word ( Word8, Word64 )
import qualified Data.ByteString.Lazy as LBS
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
import qualified Test.Tasty.ExpectedFailure as T

import Dismantle.Testing.Parser

import Prelude

-- | Configuration to drive the shared testing infrastructure
data ArchTestConfig = forall i .
  ATC { disassemble :: LBS.ByteString -> (Int, Maybe i)
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
      , archName :: String
      -- ^ A name for the ISA to appear in test output
      , ignoreAddresses :: [(FilePath, [Word64])]
      -- ^ A list of files and addresses in those files to ignore. This
      -- is typically used when we know that some locations contain data
      -- bytes and we don't want to test instruction parses of those
      -- bytes.
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

-- | Build a test case for parsing and pretty printing for the specified
-- input. If this test case should be skipped, return Nothing.
mkTestCase :: ArchTestConfig
           -- ^ The testing configuration to use.
           -> FilePath
           -- ^ The path to the file being read.
           -> Word64
           -- ^ The address of the instruction being parsed.
           -> LBS.ByteString
           -- ^ The instruction byte sequence.
           -> TL.Text
           -- ^ The objdump pretty-print output for this instruction.
           -> Maybe T.TestTree
mkTestCase cfg file addr bytes txt
    | not $ addressIsIgnored cfg file addr =
        case cfg of
          ATC { disassemble = disasm
              , assemble = assm
              , prettyPrint = pp
              , skipPrettyCheck = skipPPRE
              , expectFailure = expectFailureRE
              , normalizePretty = norm
              } ->
            let tc = insnTestCase norm disasm assm pp skipPPRE bytes txt
            in Just $ case maybe False (txt RE.=~) expectFailureRE of
              False -> tc
              True -> T.expectFail tc
    | otherwise = Nothing

insnTestCase :: (TL.Text -> TL.Text)
             -> (LBS.ByteString -> (Int, Maybe i))
             -> (i -> LBS.ByteString)
             -> (i -> PP.Doc)
             -> Maybe RE.RE
             -> LBS.ByteString
             -> TL.Text
             -> T.TestTree
insnTestCase normalize disasm asm pp skipPrettyRE bytes txt = T.testCase (TL.unpack txt) $ do
  let (_consumed, minsn) = disasm bytes
  case minsn of
    Nothing -> T.assertFailure (printf "Failed to disassemble %s (%s)" (binaryRep bytes) (TL.unpack txt))
    Just i -> do
      let roundtripMsg = printf "Roundtrip %s (parsed as %s):\n\tOriginal Bytes:%s\n\tReassembled As:%s" (show txt) (show (pp i)) (binaryRep bytes) (binaryRep (asm i))
      T.assertBool roundtripMsg (bytes == asm i)
      unless (maybe False (txt RE.=~) skipPrettyRE) $ do
        let prettyMsg = printf "Pretty Printing comparison failed.\n\tExpected: '%s'\n\tActual:   '%s'" (TL.unpack txt) (show (pp i))
        T.assertBool prettyMsg (normalize txt == normalize (TL.pack (show (pp i))))

-- | Given an architecture-specific configuration and a directory containing
-- binaries, run @objdump@ on each binary and then try to disassemble and
-- re-assemble each instruction in those binaries (as identified by @objdump@).
--
-- Additionally, the output of the automatically-generated pretty printer is
-- compared against the output of @objdump@ unless the 'skipPrettyCheck' regex
-- matches the instruction under test.
binaryTestSuite :: ArchTestConfig -> FilePath -> IO T.TestTree
binaryTestSuite atc dir = do
  testsByFile <- withInstructions objdumpParser dir (instructionFilter atc) (mkTestCase atc)

  -- Filter out the Nothing test cases for each file, since those were
  -- ignored by the ArchTestConfig. If no tests remain for a given file
  -- (admittedly very unlikely), just skip that file when building
  -- testGroups.
  let mTestCases = flip map testsByFile $ \(f, mTests) ->
        let tests = catMaybes mTests
        in if null tests
           then Nothing
           else Just $ T.testGroup f tests

  return (T.testGroup (archName atc) (catMaybes mTestCases))

-- | Convert a directory of executables into a list of data, where
-- each data item is constructed by a callback called on one
-- instruction disassembled by objdump.
withInstructions :: Parser Disassembly
                 -- ^ The parser to use to parse the objdump output
                 -> FilePath
                 -- ^ A directory containing executables (that can be objdumped)
                 -> (Instruction -> Bool)
                 -- ^ Instruction filter
                 -> (FilePath -> Word64 -> LBS.ByteString -> TL.Text -> a)
                 -- ^ Turn a disassembled instruction into data
                 -> IO [(FilePath, [a])]
withInstructions parser dir filterInstruction con = do
  files <- namesMatching (dir </> "*")
  mapM disassembleFile files
  where
    disassembleFile f = do
      insns <- withDisassembledFile parser f $ \d -> do
        T.forM (filter filterInstruction $ concatMap instructions (sections d)) $ \i ->
            return (con f (insnAddress i) (insnBytes i) (insnText i))
      return (f, insns)

withDisassembledFile :: Parser Disassembly -> FilePath -> (Disassembly -> IO a) -> IO a
withDisassembledFile parser f k = do
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
    p0 = Proc.proc "objdump" ["-d", f]
    p1 = p0 { Proc.std_out = Proc.CreatePipe
            }

showByte :: Word8 -> String
showByte b =
    let s = showIntAtBase 2 intToDigit b ""
        padding = replicate (8 - length s) '0'
    in padding <> s

binaryRep :: LBS.ByteString -> String
binaryRep bytes = L.intercalate "." $ showByte <$> LBS.unpack bytes
