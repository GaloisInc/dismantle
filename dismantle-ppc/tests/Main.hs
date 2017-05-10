module Main ( main ) where

import Control.Monad ( unless )
import Data.Char ( isSpace )
import qualified Data.List as L
import Data.Word ( Word64 )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as T
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Text.Printf ( printf )
import qualified Text.RE.TDFA as RE

import Dismantle.Testing
import Parser ( p )

import qualified Dismantle.PPC as PPC

main :: IO ()
main = do
  testsByFile <- withInstructions p "tests/bin" mkTest
  let testCases = map (\(f, tests) -> T.testGroup f tests) testsByFile
  T.defaultMain (T.testGroup "PPC Tests" testCases)

mkTest :: Word64 -> LBS.ByteString -> T.Text -> T.TestTree
mkTest _addr bytes txt = T.testCase (T.unpack txt) $ do
  let (_consumed, minsn) = PPC.disassembleInstruction bytes
  case minsn of
    Nothing -> T.assertFailure ("Failed to disassemble " ++ show txt)
    Just i -> do
      let assembleMsg = printf "Reassembly of %s (actual is %s)" (show (PPC.ppInstruction i)) (T.unpack txt)
      T.assertEqual assembleMsg bytes (PPC.assembleInstruction i)
      unless (skipPrettyCheck txt) $ do
        T.assertEqual "pretty" (normalizeText txt) (normalizeText (T.pack (show (PPC.ppInstruction i))))

normalizeText :: T.Text -> T.Text
normalizeText = T.filter (not . isSpace)

-- | We want to skip some of these tests because they rely on things we just
-- can't support in this context.  For example, IP-relative jumps refer to
-- symbol names that we just can't get here.
skipPrettyCheck :: T.Text -> Bool
skipPrettyCheck t = t RE.=~ ignoreRegex

-- | A regular expression matching any instruction that we shouldn't test for
-- pretty printing accuracy.
ignoreRegex :: RE.RE
ignoreRegex = rx (L.intercalate "|" rxes)
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

           -- FIXME: The following two instructions have incorrect operand specs
           -- in the tablegen data.  Investigate upstream
           , "^[[:space:]]*mtfsb0[[:space:]]"
           , "^[[:space:]]*mtfsb1[[:space:]]"
           ]

rx :: String -> RE.RE
rx s =
  case RE.compileRegex s of
    Nothing -> error ("Invalid regex: " ++ s)
    Just r -> r

