{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Data.Char (isSpace)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Data.Text.Lazy as TL
import qualified Text.PrettyPrint as PP

import Dismantle.Testing
import Dismantle.Testing.ParserTests (parserTests)
import qualified Dismantle.Testing.Regex as RE

import qualified Dismantle.PPC.ISA as PPC
import qualified Dismantle.PPC.Disassembler as PPC
import qualified Dismantle.PPC.Assembler as PPC
import qualified Dismantle.PPC.PrettyPrint as PPC

ppc :: ArchTestConfig
ppc = ATC { testingISA = PPC.isa
          , disassemble = PPC.disassembleInstruction
          , assemble = PPC.assembleInstruction
          , prettyPrint = PPC.ppInstruction
          , expectFailure = Nothing
          , customObjdumpArgs = []
          , skipPrettyCheck = Just skipPrettyRE
          , ignoreAddresses = []
          , normalizePretty = normalize
          , comparePretty = Nothing
          , instructionFilter = const True
          }

main :: IO ()
main = do
  tg <- binaryTestSuite ppc "tests/bin"
  pt <- parserTests
  T.defaultMain $ T.testGroup "dismantle-ppc" [tg, pt, overflowOpcodeTests]

overflowOpcodeTests :: T.TestTree
overflowOpcodeTests =
  T.testGroup "overflow arithmetic opcodes"
    [ opcodeTest [0x7d, 0x04, 0x2e, 0x14] "addo r8, r4, r5"
    , opcodeTest [0x7d, 0x04, 0x2c, 0x14] "addco r8, r4, r5"
    , opcodeTest [0x7d, 0x04, 0x2c, 0x15] "addco. r8, r4, r5"
    , opcodeTest [0x7d, 0x04, 0x2d, 0x14] "addeo r8, r4, r5"
    , opcodeTest [0x7d, 0x04, 0x2d, 0x15] "addeo. r8, r4, r5"
    , opcodeTest [0x7d, 0x04, 0x05, 0xd4] "addmeo r8, r4"
    , opcodeTest [0x7d, 0x04, 0x05, 0xd5] "addmeo. r8, r4"
    , opcodeTest [0x7d, 0x04, 0x05, 0x94] "addzeo r8, r4"
    , opcodeTest [0x7d, 0x04, 0x05, 0x95] "addzeo. r8, r4"
    , opcodeTest [0x7d, 0x04, 0x2c, 0x10] "subfco r8, r4, r5"
    , opcodeTest [0x7d, 0x04, 0x2c, 0x11] "subfco. r8, r4, r5"
    , opcodeTest [0x7d, 0x04, 0x2d, 0x10] "subfeo r8, r4, r5"
    , opcodeTest [0x7d, 0x04, 0x2d, 0x11] "subfeo. r8, r4, r5"
    , opcodeTest [0x7d, 0x04, 0x2c, 0x50] "subfo r8, r4, r5"
    , opcodeTest [0x7d, 0x04, 0x2c, 0x51] "subfo. r8, r4, r5"
    , opcodeTest [0x7d, 0x04, 0x04, 0xd0] "nego r8, r4"
    , opcodeTest [0x7d, 0x04, 0x04, 0xd1] "nego. r8, r4"
    , opcodeTest [0x7d, 0x04, 0x2d, 0xd6] "mullwo r8, r4, r5"
    , opcodeTest [0x7d, 0x04, 0x2d, 0xd7] "mullwo. r8, r4, r5"
    ]

opcodeTest :: [Word] -> String -> T.TestTree
opcodeTest bytes expected =
  T.testCase expected $
    case PPC.disassembleInstruction (LBS.pack (map fromIntegral bytes)) of
      (4, Just instruction) ->
        T.assertEqual "pretty-printed instruction"
          expected
          (PP.render (PPC.ppInstruction instruction))
      result ->
        T.assertFailure ("failed to decode instruction: " ++ show result)

normalize :: TL.Text -> TL.Text
normalize =
    -- Then remove whitespace
    TL.filter (not . isSpace) .
    -- First, trim any trailing comments
    (fst . TL.breakOn ";")

-- | A regular expression matching any instruction that we shouldn't test for
-- pretty printing accuracy.
skipPrettyRE :: RE.Regex
skipPrettyRE = rx (L.intercalate "|" rxes)
  where
    rxes = [ -- IP-relative addressing references names in <>, and we can't
             -- match those correctly yet
             "<"
           -- The OR instruction is aliased to MR if the destination
           -- is the same as the second register.  We don't have a
           -- definition for MR, so we skip validating it
           , "^[[:space:]]*mr\\.?[[:space:]]"
           -- CLRLWI is a specialized form of RLWINM, but we don't have a def
           -- for it
           , "^[[:space:]]*clrlwi[[:space:]]"
           -- NOT is rendered as NOR with three operands, but we don't have a
           -- NOT def
           , "^[[:space:]]*not[[:space:]]"
           -- We render two extra zero operands with MTFSF compared to objdump
           , "^[[:space:]]*mtfsf[[:space:]]"
           -- CRCL is an alias for CRXOR
           , "^[[:space:]]*crcl[[:space:]]"
           -- Objdump renders CMPWI without its first operand if it is cr0
           , "^[[:space:]]*cmpwi[[:space:]]"
           --  ROTLWI is an alias for RLWINM
           , "^[[:space:]]*rotlwi[[:space:]]"
           -- ROTLW is an alias for RLWNM
           , "^[[:space:]]*rotlw[[:space:]]"
           -- CRCLR is an alias of CRXOR
           , "^[[:space:]]*crclr[[:space:]]"
           -- BLELR is an alias of BCL
           , "^[[:space:]]*blelr[[:space:]]"
           -- BEQLR is an alias of BCL
           , "^[[:space:]]*beqlr[[:space:]]"
           -- BNELR is an alias for BCL
           , "^[[:space:]]*bnelr[[:space:]]"
           -- BLTLR is an alias for BCL
           , "^[[:space:]]*bltlr[[:space:]]"
           -- CRSET is an alias for CREQV
           , "^[[:space:]]*crset[[:space:]]"
           -- CRNOT is an alias for CRNOR
           , "^[[:space:]]*crnot[[:space:]]"
           -- LWSYNC is an alias for SYNC 1
           , "^[[:space:]]*lwsync"
           -- MFTBU and MFTB have implicit operands that our pretty printer
           -- shows, but objdump does not.
           , "^[[:space:]]*mftbu?[[:space:]]"

           -- FIXME: The following two instructions have incorrect operand specs
           -- in the tablegen data.  Investigate upstream
           , "^[[:space:]]*mtfsb0[[:space:]]"
           , "^[[:space:]]*mtfsb1[[:space:]]"
           ]

rx :: String -> RE.Regex
rx s =
  case RE.mkRegex s of
    Left e -> error ("Invalid regex <<" ++ s ++ ">> because: " ++ e)
    Right r -> r
