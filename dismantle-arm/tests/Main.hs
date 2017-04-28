module Main ( main ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as T
import Data.Word ( Word64 )
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import Dismantle.Testing

import Dismantle.ARM as ARM

main :: IO ()
main = do
  testsByFile <- withInstructions "tests/bin" mkTest
  let testCases = map (\(f, tests) -> T.testGroup f tests) testsByFile
  T.defaultMain (T.testGroup "ARM Tests" testCases)

mkTest :: Word64 -> LBS.ByteString -> T.Text -> T.TestTree
mkTest _addr bytes txt = T.testCase (T.unpack txt) $ do
  let (_consumed, minsn) = ARM.disassembleInstruction bytes
  case minsn of
    Nothing -> T.assertFailure ("Failed to disassemble " ++ show txt)
    Just i -> T.assertEqual "Reassembly" bytes (ARM.assembleInstruction i)
