module Main ( main ) where

import Data.Word ( Word64 )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as T
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Text.Printf ( printf )

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
    Just i ->
      let msg = printf "Reassembly of %s" (show (PPC.ppInstruction i))
      in T.assertEqual msg bytes (PPC.assembleInstruction i)

{-

FIXME: Add a test suite that accepts an assembly string and compares the values
of disassembled operands to expected values.  This will help keep operand
encoding/decoding debugging simple.

-}

