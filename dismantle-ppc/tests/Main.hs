module Main ( main ) where

import Control.Monad ( unless )
import Data.Char ( isSpace )
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
skipPrettyCheck t = or [ T.pack "<" `T.isInfixOf` t
                       -- The OR instruction is aliased to MR if the destination
                       -- is the same as the second register.  We don't have a
                       -- definition for MR, so we skip validating it
                       , t RE.=~ rx "^[[:space:]]*mr"
                       -- clrlwi is a specialized form of rlwinm, but we don't
                       -- have a definition for it.
                       , t RE.=~ rx "^[[:space:]]*clrlwi"
                       ]

rx :: String -> RE.RE
rx s =
  case RE.compileRegex s of
    Nothing -> error ("Invalid regex: " ++ s)
    Just r -> r

