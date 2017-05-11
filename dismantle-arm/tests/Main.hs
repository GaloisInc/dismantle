module Main ( main ) where

import qualified Test.Tasty as T
import qualified Text.RE.TDFA as RE

import Dismantle.Testing

import qualified Dismantle.ARM as ARM

arm :: ArchTestConfig
arm = ATC { archName = "arm"
          , disassemble = ARM.disassembleInstruction
          , assemble = ARM.assembleInstruction
          , prettyPrint = ARM.ppInstruction
          , expectFailure = Nothing
          , skipPrettyCheck = Just (rx ".*")
          }

main :: IO ()
main = do
  tg <- binaryTestSuite arm "tests/bin"
  T.defaultMain tg

rx :: String -> RE.RE
rx s =
  case RE.compileRegex s of
    Nothing -> error ("Invalid regex: " ++ s)
    Just r -> r
